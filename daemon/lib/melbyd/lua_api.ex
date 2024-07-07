defmodule Melbyd.LuaInterop do
  @moduledoc false

  def run(lua_file, func_path, func_args) do
    with {:ok, _, st} <- run_file(lua_file),
         {res, _st} <- Luerl.call_function(st, func_path, func_args) do
      # Now that we have the Lua state with all custom callback functions loaded
      # inside it (as well as calls to our Elixir Melbyd SDK), we can piece
      # together what the user wants.
      {:ok, res}
    else
      reason -> {:error, "#{inspect(reason)}"}
    end
  end

  def run_file(lua_file) do
    with st0 <- Luerl.init(),
         # Expose Melbyd API functions (everything inside Melbyd.LuaSdk).
         st1 <- Luerl.load_module(st0, ["melbyd"], Melbyd.LuaSdk),
         # We must run "dofile" because otherwise nothing is loaded. That is, if
         # the Lua file has "function ... end" definitions and nothing else, the
         # only way to load these functions is to run Luerl.dofile/1 here. If we
         # use Luerl.loadfile/1 instead, the custom Lua functions are not loaded
         # into the state.
         {res, st2} <- Luerl.dofile(st1, String.to_charlist(lua_file)) do
      {:ok, res, st2}
    else
      reason -> {:error, "#{inspect(reason)}"}
    end
  end
end

import Cachex.Spec

defmodule Melbyd.LuaConfigValidation do
  require Logger

  # FIXME: Add Cachex lookup to see if this config was validated previously. We
  # should add this lookup after we've implemented the validation functionality
  # completely.
  #
  # FIXME: (For caching) We should require env vars (which we use) to be
  # declared ahead of time, and hash the *names* of these env vars (not their
  # contents) as required inputs (you can think of them as types) to a function
  # (this Lua config). And then if the config does not declare this list of env
  # vars, we can refuse to validate it. This way we capture the exact "function
  # definition", so to speak, of the Lua config by precisely describing its
  # definition.
  def validate(lua_file) do
    with st0 <- Luerl.init(),
         st1 <- Luerl.load_module(st0, ["melbyd"], Melbyd.LuaSdk),
         # Overwrite "read_standard_resource" with the fake version. Note that
         # this __handle_lua_call__... function is defined by the def_lua_func
         # macro for the "read_standard_resource_fake" function. We have to use
         # Luerl.set_table1/3 (with the tuple form of {:erl_func, func_ref}) and
         # not Luerl.set_table/3 (which we can use with just the func_ref alone
         # as the 3rd argument --- this passes compilation), because we already
         # perform encoding back for Luerl inside all __handle_lua_call__...
         # functions. Otherwise we would be encoding twice.
         st2 <-
           Luerl.set_table1(
             st1,
             ["melbyd", "read_standard_resource"],
             {:erl_func, &Melbyd.LuaSdk.__handle_lua_call__read_standard_resource_fake/2}
           ),
         # Overwrite the "get_path_aliases" function to use the contents of the
         # variable directly (instead of doing I/O to read the given path).
         st3 <-
           Luerl.set_table1(
             st2,
             ["melbyd", "get_path_aliases"],
             {:erl_func, &Melbyd.LuaSdk.__handle_lua_call__get_path_aliases_fake/2}
           ),
         {_, st4} <- Luerl.dofile(st3, String.to_charlist(lua_file)),
         vm_fingerprint <- get_vm_fingerprint(st4),
         {res, view_params_types} <-
           Cachex.fetch(:lua_config_validation_cache, {lua_file, vm_fingerprint}) do
      case res do
        :ok -> {:ok, view_params_types}
        :commit -> {:ok, view_params_types}
        :ignore -> {:error, "failed cache key validation"}
        :error -> {:error, "failed cache key validation2"}
      end
    else
      e -> {:error, e}
    end
  end

  # Return the blank state of the initial Lua VM state meant for validation
  # purposes only.
  #
  # FIXME: Add a fake for SPS also? And also delete it when we're done with
  # validation?
  def preloaded_fake_lua_vm() do
    Luerl.init()
    |> Luerl.load_module(["melbyd"], Melbyd.LuaSdk)
    # Overwrite "read_standard_resource" with the fake version. Note that this
    # __handle_lua_call__... function is defined by the def_lua_func macro for
    # the "read_standard_resource_fake" function. We have to use
    # Luerl.set_table1/3 (with the tuple form of {:erl_func, func_ref}) and not
    # Luerl.set_table/3 (which we can use with just the func_ref alone as the
    # 3rd argument --- this passes compilation), because we already perform
    # encoding back for Luerl inside all __handle_lua_call__... functions.
    # Otherwise we would be encoding twice.
    |> Luerl.set_table1(
      ["melbyd", "read_standard_resource"],
      {:erl_func, &Melbyd.LuaSdk.__handle_lua_call__read_standard_resource_fake/2}
    )
    # Overwrite the "get_path_aliases" function to use the contents of the
    # variable directly (instead of doing I/O to read the given path).
    |> Luerl.set_table1(
      ["melbyd", "get_path_aliases"],
      {:erl_func, &Melbyd.LuaSdk.__handle_lua_call__get_path_aliases_fake/2}
    )
  end

  def validate_key({lua_file, vm_fingerprint}, preloaded_fake_lua_vm) do
    # We already declare the env var names we want to use in
    # our config, so we don't actually need the env_vars at all to be part of
    # the cache key..! So just
    # reading the lua_file (which itself declares the exact env vars we need) is
    # enough.

    # For deletion of the side effects (SRS, etc) generated via validation, the
    # strategy is to use the vm_fingerprint. We store the fingerprint inside the
    # Lua VM state. Then when the fake versions of read_standard_resource are
    # called, they save this vm_fingerprint as part of their ID. Then we call a
    # "cleanup side effects" function directly right here with the
    # vm_fingerprint. Then this cleanup function broadcasts a shutdown message
    # to all resources with this vm_fingerprint. We have to only delete the
    # fakes that were generated as part of validing this particular
    # vm_fingerprint, because otherwise we would be deleting other resources
    # that might be in the middle of validating a different Lua config.
    with {_, st1} <- Luerl.dofile(preloaded_fake_lua_vm, String.to_charlist(lua_file)),
         view_params_types <- get_view_params_types(st1),
         :ok <- assert_expected_views(st1, view_params_types, vm_fingerprint) do
      # FIXME: Maybe store some more useful metrics beyond just
      # "view_params_types". E.g., the number of test cases executed, how long
      # it took to run it (at what time), etc.
      {:commit, view_params_types}
    else
      # If there's any error, abort and return nil.
      err ->
        Logger.warning("got error: #{inspect(err)}")
        {:ignore, nil}
    end
  end

  # This checks the types of the actual env_vars from the real environment.
  def get_view_params_types(st0) do
    {view_params_types_tuples, _st} = Luerl.get_table(st0, ["Config", "view_params_types"])

    view_params_types =
      view_params_types_tuples
      |> Melbyd.LuerlUtil.table_to_native_map()

    Map.put(
      view_params_types,
      "env",
      view_params_types["env"]
      |> Melbyd.LuerlUtil.table_to_native_map()
    )
  end

  def enforce_view_params_types(view_params_types, env_vars) do
    errors =
      Enum.reduce(view_params_types["env"], [], fn {name, type_array}, errors ->
        [req_or_opt, type] =
          type_array
          |> Melbyd.LuerlUtil.array_to_native_list()

        required = req_or_opt == "required"

        # Check vals of real env_vars.
        case assert_env_var_type(env_vars, name, required, type) do
          :ok -> errors
          {:error, reason} -> ["#{name}: " <> reason | errors]
        end
      end)

    if errors == [] do
      # Reduce the env_vars map to only include the env_vars listed in view_params_types.
      env_vars_reduced =
        Enum.reduce(env_vars, %{}, fn {k, _}, env_vars_reduced ->
          val = Map.fetch!(env_vars, k)
          Map.put(env_vars_reduced, k, val)
        end)

      {:ok, env_vars_reduced}
    else
      {:error, Enum.join(errors, "\n")}
    end
  end

  def assert_env_var_type(env_vars, name, required, type) do
    with {:ok, val} <- Map.fetch(env_vars, name),
         :ok <- assert_maybe_required(name, val, required) do
      case type do
        "path" ->
          assert_type_path(val)

        "paths" ->
          assert_type_paths(val)

        # The contents of a file can be anything (any sequence of bytes), so it's always well-formed.
        "pathblob" ->
          :ok

        "int" ->
          assert_type_int(val, false)

        "uint" ->
          assert_type_int(val, true)

        # Expect the string to be composed of at least 1 non-whitespace character.
        "string" ->
          if String.trim(val) == "" do
            {:error, "string must have at least 1 non-whitespace character"}
          else
            :ok
          end

        t ->
          {:error, "unrecognized type #{t}"}
      end
    else
      err ->
        case err do
          :error ->
            if required do
              {:error, "env var #{name} required but does not exist"}
            else
              :ok
            end

          e ->
            {:error, "urecognized error #{inspect(e)}"}
        end
    end
  end

  def assert_maybe_required(name, val, required) do
    if required do
      if String.length(val) == 0 do
        {:error, "env var #{name} is required, but it is set to the empty string"}
      else
        :ok
      end
    else
      :ok
    end
  end

  def assert_type_int(maybe_int, unsigned) do
    try do
      int = String.to_integer(maybe_int)

      if unsigned do
        if int < 0 do
          {:error, "not a uint: #{maybe_int} is negative"}
        else
          :ok
        end
      else
        :ok
      end
    rescue
      e -> {:error, "not an integer: #{maybe_int}: #{Exception.message(e)}"}
    end
  end

  def assert_type_path(path) do
    # FIXME: use haskell parsec to do a full parse

    # Use a basic heuristic. Assert that the first character starts with a slash
    # "/", that the last character is not a slash, and that there are no runs of
    # consecutive slashes. No null bytes. Last character cannot  be "." (this
    # includes the case where the last 2 characters are ".").

    cond do
      String.length(path) == 0 -> {:error, "cannot be empty"}
      String.first(path) != "/" -> {:error, "first character must be a slash"}
      String.last(path) == "/" -> {:error, "last character cannot be a slash"}
      String.last(path) == "." -> {:error, "last character cannot be a dot"}
      String.contains?(path, "//") -> {:error, "consecutive slashes are forbidden"}
      String.contains?(path, "\0") -> {:error, "cannot contain a null byte"}
      true -> :ok
    end
  end

  def assert_type_paths(paths) do
    # FIXME: use haskell parsec to do a full parse

    # Use a basic heuristic. Assert that the first character starts with a slash
    # "/", that the last character is not a slash, and that there are no runs of
    # consecutive slashes. No null bytes. Last character cannot  be "." (this
    # includes the case where the last 2 characters are ".").

    error_reasons =
      String.split(paths, ":")
      |> Enum.reduce([], fn path, error_reasons ->
        case assert_type_path(path) do
          {:error, reason} -> [reason | error_reasons]
          _ -> error_reasons
        end
      end)

    if error_reasons == [] do
      :ok
    else
      {:error, error_reasons}
    end
  end

  # FIXME: Each of the SRS models have a "fake" field with faked readers in it
  # (which return plausible, well-formatted data). We just have to use them
  # somehow.
  #
  # We can make this fake tick runner just run the ["Config", "view"] function
  # in the Lua state, wait 1 second, check the result and compare it to the
  # expectation, and repeat. The expectations can come from the config file
  # itself (add it under another key path). The behind the scenes the
  # StandardResource should behave almost like in prod, but with the difference
  # that it'll run the fake readers instead of the actual readers, as well as
  # provision the SRS GenServer in a different namespace so as not to clash with
  # the production resources.

  # The config should tell us how many "ticks" and corresponding expectations to
  # run. We could enforce a minimum of 3 rounds from our side (i.e., fail
  # validation if the user has not configured at lesat 3 rounds of
  # expectations).
  #
  # Each iteration should be:
  #
  #     1. Execute ["Config", "view"] function in the Lua VM state (time_idx is 1).
  #
  #     2. Expectation: check expected return value of the above versus what we
  #     actually got (got vs want).
  #
  #     3. Increment time_idx by 1, and re-run step 1. Continue until time_idx
  #     == 10 or some other number.
  def assert_expected_views(st0, view_params_types, vm_fingerprint) do
    # Set the vm_fingerprint inside the Lua VM, so that the fake Lua SDK
    # functions can use them to pass them on to Elixir.
    st1 = Luerl.set_table1(st0, ["melbyd", "vm_fingerprint"], vm_fingerprint)

    # 'expectations' is a list of list of substrings we need to match after
    # calling ["Config", "view"].
    {view_tests_tuples, _st} = Luerl.get_table(st1, ["Config", "view_tests"])

    view_tests =
      view_tests_tuples
      |> Melbyd.LuerlUtil.array_to_native_list()
      |> Enum.map(&Melbyd.LuerlUtil.table_to_native_map/1)

    # For each expectation in the expectations, run the Config.view() function.
    # Because we are running fake resources, the read() call will be synchronous
    # and force a re-read, incrementing the :read field in every SRS each time.
    errors =
      Enum.reduce(view_tests, [], fn view_test, errors ->
        # Check vals of fake env_vars in view_tests.
        env_vars_fake = view_test["env"] |> Melbyd.LuerlUtil.table_to_native_map()

        # FIXME: dedupe this logic (copied from enforce_view_params_types)
        type_errors =
          Enum.reduce(view_params_types["env"], [], fn {name, type_array}, errors ->
            [req_or_opt, type] =
              type_array
              |> Melbyd.LuerlUtil.array_to_native_list()

            required = req_or_opt == "required"

            # Check vals of real env_vars.
            case assert_env_var_type(env_vars_fake, name, required, type) do
              :ok -> errors
              {:error, reason} -> ["#{name} (view_test): " <> reason | errors]
            end
          end)

        if type_errors == [] do
          case assert_expected_view(st1, view_test, env_vars_fake) do
            :ok -> errors
            {:error, errs} -> [errs | errors]
          end
        else
          type_errors ++ errors
        end
      end)

    # Send message to delete all fake SRS GenServers, because we're done using
    # them. We won't need them until we need to run this function again, which
    # will only happen when the cache entry for this vm_fingerprint expires
    # (which should technically only happen when melbyd restarts).
    Phoenix.PubSub.broadcast(
      Melbyd.PubSub,
      "fake_" <> vm_fingerprint,
      {:EXIT, self(), :release_fake_resource}
    )

    if Kernel.length(errors) > 0 do
      Logger.warning("got #{Kernel.length(errors)} errors reading fake readers")
      errors |> Enum.map(fn e -> Logger.warning(e) end)
      "failed validation"
    else
      :ok
    end
  end

  defp assert_expected_view(st0, view_test, env_vars_fake) do
    substrings = view_test["expected_substrings"] |> Melbyd.LuerlUtil.array_to_native_list()

    {[got], _st} = Luerl.call_function(st0, ["Config", "view"],
                                       [env_vars_fake, "000"])

    errors =
      Enum.reduce(substrings, [], fn substring, errors ->
        if String.contains?(got, substring) do
          errors
        else
          ["could not find #{inspect(substring)} inside #{inspect(got)}" | errors]
        end
      end)

    if errors == [] do
      :ok
    else
      {:error, errors}
    end
  end

  def get_vm_fingerprint({a, b, c, d, e, f, g, h, i, _j, _k, l, m}) do
    {a, b, c, d, e, f, g, h, i, l, m}
    |> Kernel.inspect(
      limit: :infinity,
      printable_limit: :infinity,
      width: :infinity
    )
    |> (&:crypto.hash(:sha, &1)).()
    # Make this fingerprint easier to debug.
    |> Base.encode16()
  end

  # Cache for storing a boolean of whether this config has already been
  # validated or not.
  @cache_id :lua_config_validation_cache

  def child_spec(_init_arg) do
    %{
      id: @cache_id,
      type: :supervisor,
      start:
        {Cachex, :start_link,
         [
           @cache_id,
           [
             limit: 16,
             fallback: fallback(default: &validate_key/2, state: preloaded_fake_lua_vm())
           ]
         ]}
    }
  end
end
defmodule Melbyd.LuaSdkLoadable do
  require Logger

  # This prevents programmer errors where we no longer autogenerate an install/1
  # function. This will probably never happen, but because we don't have types
  # this is the best we can do.
  @callback install(Lua.t()) :: Lua.t()

  alias Melbyd.LuaSdkLoader, as: Loader

  defmacro __using__(_options \\ []) do
    quote do
      import unquote(__MODULE__)
      Module.register_attribute(__MODULE__, :loadable_functions, accumulate: true)
      @before_compile unquote(__MODULE__)
    end
  end

  # Maybe this macro should have been named "final_macro_expansion" or
  # something. But the point is that we run this after all other macros are
  # expanded, *just* before compilation to Erlang bytecode begins.
  defmacro __before_compile__(_env) do
    quote do

      # install/1 is called by Luerl's load_module() function. This is documented in https://github.com/rvirding/luerl/blob/bc655178dc8f59f29199fd7df77a7c314c0f2e02/src/NOTES#L115.
      def install(st) do
        table = Loader.load(@loadable_functions, __MODULE__)
        :luerl_heap.alloc_table(table, st)
      end
    end
  end

  # Note that all Elixir expressions, when quoted (converted into an AST), have
  # the form of {atom, context, arguments}.
  #
  # Here we pattern match the arguments so that we only process those function
  # declarations that want 2 arguments, because that's what Luerl requires of
  # us. (First argument is a  list of arguments passed in from the invocation of
  # this function from Lua, and the second argument is the Lua VM state.)
  #
  # We define 2 functions for every def_lua_func macro. The first is a private
  # function that actually captures the business logic. The second is a wrapper
  # that makes sure that the function behaves as a proper Luerl-compatible
  # function, in that it always returns things through the Loader.to_luerl/1
  # helper function. We basically use this to force all functions to behave
  # properly.
  #
  # "dlf_block" means "def_lua_func block".
  defmacro def_lua_func({f_name, f_context, [_args_from_lua, _st] = f_args} = _elixir_expression,
             do: dlf_block
           ) do
    # If a user created a function named "foo", then create another Elixir
    # function named as "__handle_lua_call__foo".
    prefixed_f_name = String.to_atom("__handle_lua_call__" <> Atom.to_string(f_name))
    prefixed_f_name_internal = String.to_atom("__internal__" <> Atom.to_string(f_name))

    quote do
      # Register this function call (make a note of it) into the
      # @loadable_functions attribute.
      @loadable_functions unquote(prefixed_f_name)

      # This is the wrapper function that is publicly visible to Luerl. It runs
      # the business logic and makes sure to wrap the return values in a form
      # that Luerl expects by piping to Loader.to_luerl/1.
      def unquote(prefixed_f_name)(args_list_from_lua, lua_state) do
        unquote(prefixed_f_name_internal)(args_list_from_lua, lua_state)
          |> Loader.to_luerl
      end

      # This is the private function that actually houses the business logic.
      defp unquote({prefixed_f_name_internal, f_context, f_args}) do
        # Define the function body (written by the user in the "do ... end"
        # part) as-is.
        unquote(dlf_block)
      end
    end
  end
end

# The point of this module is so that we can minimize our use of macros. This
# way, we minimize the amount of code we generate dynamically to only generate
# the bare minimum to get things working.
defmodule Melbyd.LuaSdkLoader do

  def load(elixir_funcs, module) do
    Enum.map(elixir_funcs, fn elixir_func ->
      # From the Lua side, we can call an Elixir function by its "short" name,
      # without the leading "__handle_lua_call__" prefix.
      lua_func_name =
        String.replace_prefix(Atom.to_string(elixir_func), "__handle_lua_call__", "")

      # The arity is fixed at 2, because (FIXME: add link).
      elixir_func_reference = Function.capture(module, elixir_func, 2)
      {lua_func_name, {:erl_func, elixir_func_reference}}
    end)
  end

  # Helper function for returning things back to Luerl (Lua VM) from an autoloaded Elixir
  # module.
  #
  # Through trial and error, it appears that the return value must be a tuple
  # where the first element is what's returned to Lua and the second element is
  # the new state of the Lua VM.
  #
  # The first element itself has to be a list of 2 elements, of the form
  # [lua_return_value, error]. If we don't do this we get a strange error from
  # Elixir. The error, if not nil, must be a string.
  #
  # For consistency with idiomatic Elixir, we make callers use the familiar
  # "{:ok, foo}" and "{:error, message}" patterns and translate them here to
  # make luerl happy.
  def to_luerl(val) do
    case val do
      {:ok, result, st} ->
        # We need to encode results before the Lua functions can use them.
        {result_encoded, st1} = Luerl.encode(st, result)
        {[result_encoded, nil], st1}
      {:error, msg, st} -> {[nil, msg], st}
      got -> raise "got #{got}, expected {:ok, result, st} or {:error, msg, st}"
    end
  end
end
defmodule Melbyd.LuaSdk do
  @moduledoc """
  An Elixir module that can be accessed from Lua (via luerl).
  """

  # This declaration just prevents programmer errors where we forget to define
  # install/1.
  @behaviour Melbyd.LuaSdkLoadable

  # Autogenerate some boilerplate to avoid having to manually define
  # autoloaded_functions_table(). Specifically it defines the install/1 and
  # loaded_functions_table/0 for us automatically.
  use Melbyd.LuaSdkLoadable

  require Logger

  @newlines ["\n", "\r", "\r\n", "\n\r"]

  def get_melbyr_addr() do
    "localhost:#{Application.get_env(:melbyd, :melbyr_port)}"
  end

  def_lua_func render([widgets_list_ref, delimiter_ref], st0) do
    delimiter_tuples = Luerl.decode(st0, delimiter_ref)
    delimiter = widget_from_tuples(delimiter_tuples)
  
    # Retrieve render_options from the Config table in the Lua state.
    render_options = get_render_options(st0)
  
    widgets =
      Luerl.decode(st0, widgets_list_ref)
      |> Enum.map(fn {_idx, widget_tuples} ->
        widget_from_tuples(widget_tuples)
      end)
  
    req = %MelbyRenderer.RenderWidgetsRequest{}
    req = Map.put(req, :widgets, widgets)
    req = Map.put(req, :delimiter, delimiter)
    req = Map.put(req, :render_options, render_options)
  
    # Call out to melbyr over gRPC.
    addr = get_melbyr_addr()
    with {:ok, channel} <- GRPC.Stub.connect(addr),
         {:ok, reply} <- MelbyRenderer.Renderer.Stub.render_widgets(channel, req, timeout: 200) do
      GRPC.Stub.disconnect(channel)
      {:ok, reply.widgets_rendered, st0}
    else
      err -> raise "could not parse response from melbyr: #{inspect(err)}"
    end
  end
  
  def get_render_options(st0) do
    {render_options_tuples, _st1} = Luerl.get_table(st0, ["Config", "render_options"])
  
    render_options_map =
      Map.new(
        render_options_tuples
        |> Enum.map(fn {k, v} -> {String.to_atom(k), String.to_atom(v)} end)
      )
  
    render_options = %MelbyRenderer.RenderOptions{}
    Map.merge(render_options, render_options_map)
  end
  
  def widget_from_tuples(widget_tuples) do
    {widget, tp} =
      Enum.reduce(
        widget_tuples,
        {%MelbyRenderer.Widget{}, %MelbyRenderer.TextProperty{}},
        fn {k, v}, acc ->
          {acc_w, acc_tp} = acc
          # We only recognize certain keywords.
          case k do
            "str" ->
              {Map.put(acc_w, :str, v), acc_tp}
  
            "fg" ->
              {acc_w, Map.put(acc_tp, :fg, color_from_str(v))}
  
            "bg" ->
              {acc_w, Map.put(acc_tp, :bg, color_from_str(v))}
  
            "styles" ->
              styles = Enum.map(v, fn {_idx, s} -> style_from_str(s) end)
              {acc_w, Map.put(acc_tp, :styles, styles)}
  
            # Skip over any unrecognized key.
            "drop_delim_left" ->
              {Map.put(acc_w, :drop_delim_left, v), acc_tp}
  
            "drop_delim_right" ->
              {Map.put(acc_w, :drop_delim_right, v), acc_tp}
  
            _ ->
              acc
          end
        end
      )
  
    Map.put(widget, :prop, tp)
  end
  
  def color_from_str(s) do
    {r, g, b} = Melbyd.Color.parse(s)
    c24bit = %MelbyRenderer.Color24Bit{red: r, green: g, blue: b}
    %MelbyRenderer.Color{color_oneof: {:color_24_bit, c24bit}}
  end
  
  def style_from_str(s) do
    case s do
      "bold" -> :TEXT_STYLE_BOLD
      # FIXME: skip over unrecogized values
      _ -> :TEXT_STYLE_BOLD
    end
  end
  def_lua_func get_path_pretty([path, options_ref], st0) do
    options = Luerl.decode(st0, options_ref)
    options_map = Map.new(options)
  
    aliases = options_map["aliases"] |> Melbyd.LuerlUtil.table_to_native_map()
    env = options_map["env"] |> Melbyd.LuerlUtil.table_to_native_map()
  
    # Create a subset of env vars. This is because we only care about the ones used by the keys in aliases.
    {aliases_filtered, env_filtered} =
      Enum.reduce(aliases, {%{}, %{}}, fn {k, v}, {aliases_filtered, env_filtered} ->
        vars = Regex.scan(~r/\$\{(.+?)\}/, k) |> Enum.map(fn [_entire_match, group] -> group end)
        # Check if every variable is found in env.
        env_subset = Map.take(env, vars)
  
        if length(vars) == Enum.count(env_subset) do
          # Only keep aliases if we can find all of its environment variable references.
          {Map.put(aliases_filtered, k, v), Map.merge(env_filtered, env_subset)}
        else
          {aliases_filtered, env_filtered}
        end
      end)
  
    # If we were unable to parse the aliases (e.g., we are given no aliases to
    # begin with because we failed to parse the path aliases file), then just use
    # a map with the HOME variable. This is because the Rust NIF always
    # expects at least the "HOME" env var to exist.
    env_filtered_final =
      if env_filtered == %{} do
        if env["HOME"] == nil do
          Logger.warning(
            "HOME environment variable is not defined; using useless default \"/home/foo\""
          )
        end
  
        Map.new([{"HOME", env["HOME"] || "/home/foo"}])
      else
        env_filtered
      end
  
    # Default value is 0 (no shortening).
    shorten_threshold = Map.get(options_map, "shorten_threshold", 0)
  
    prettified =
      Melbyd.Path.get_path_pretty(path, aliases_filtered, env_filtered_final, shorten_threshold)
  
    {:ok, prettified, st0}
  end
  
  def_lua_func get_path_aliases([path_aliases_file], st0) do
    if path_aliases_file == nil do
      Logger.warning("path_aliases_file is nil; defaulting to empty map")
      {:ok, Map.new(), st0}
    else
      with {:ok, path_aliases_raw} <- File.read(path_aliases_file) do
        get_path_aliases_helper(path_aliases_raw, st0)
      end
    end
  end
  
  # This is just like get_path_aliases, but expects the raw file contents instead
  # of the filename (path) to read out.
  def_lua_func get_path_aliases_fake([path_aliases_raw], st0) do
    get_path_aliases_helper(path_aliases_raw, st0)
  end
  
  def get_path_aliases_helper(path_aliases_raw, st0) do
    req = %MelbyRenderer.ParsePathAliasesRequest{}
    req = Map.put(req, :path_aliases_raw, path_aliases_raw)
  
    # Call out to melbyr over gRPC.
    addr = get_melbyr_addr()
    with {:ok, channel} <- GRPC.Stub.connect(addr),
         {:ok, reply} <- MelbyRenderer.Renderer.Stub.parse_path_aliases(
           channel, req, timeout: 200) do
      GRPC.Stub.disconnect(channel)
      if reply.status == :PARSE_STATUS_ERROR do
        Logger.warning("parse failed: #{inspect(reply.error)}")
      end
      {:ok, reply.path_aliases, st0}
    else
      err -> raise "failed to successfully call melbyr: #{inspect(err)}"
    end
  end
  def_lua_func get_colorized_sha([sha, sha_length, pad_left, pad_right], st0) do
    render_options = get_render_options(st0)
  
    req = %MelbyRenderer.ColorizedGitShaRequest{}
    req = Map.put(req, :sha, sha)
    req = Map.put(req, :sha_length, sha_length)
    req = Map.put(req, :pad_left, pad_left)
    req = Map.put(req, :pad_right, pad_right)
    req = Map.put(req, :render_options, render_options)
  
    Logger.debug("elixir req was: #{inspect(req)}")
  
    addr = get_melbyr_addr()
    with {:ok, channel} <- GRPC.Stub.connect(addr),
         {:ok, reply} <- channel |> MelbyRenderer.Renderer.Stub.get_colorized_git_sha(req, timeout: 200) do
      GRPC.Stub.disconnect(channel)
      {:ok, reply.sha_colorized, st0}
    end
  end
  def_lua_func get_time([format, unix_seconds, time_zone], st0) do
    # Use UTC by default.
    tz =
      cond do
        time_zone == "LOCAL" -> Timex.Timezone.local().full_name
        timezone_ok(time_zone) -> time_zone
        true -> "Etc/UTC"
      end
  
    # Use the current time if unix_seconds is not provided.
    sec =
      if unix_seconds do
        if is_binary(unix_seconds) do
          String.to_integer(unix_seconds)
        else
          unix_seconds
        end
      else
        with {:ok, t} <- DateTime.now(tz),
             do: DateTime.to_unix(t)
      end
  
    # Format the current time with the given format string.
    t =
      DateTime.from_unix!(sec)
      |> DateTime.shift_zone!(tz)
  
    {:ok, Calendar.strftime(t, format), st0}
  end
  
  def_lua_func get_unix_seconds(_, st0) do
    with {:ok, t} <- DateTime.now("Etc/UTC"),
         do: {:ok, DateTime.to_unix(t), st0}
  end
  
  def timezone_ok(tz) do
    case DateTime.now(tz) do
      {:ok, _} ->
        true
  
      _ ->
        Logger.warning("unrecognized timezone: '#{tz}'")
        false
    end
  end
  def_lua_func to_shell_script([exports_ref], st0) do
    exports_list = Luerl.decode(st0, exports_ref) |> Melbyd.LuerlUtil.array_to_native_list()
    exports = exports_list |> Enum.map(&Melbyd.LuerlUtil.table_to_native_map/1)
  
    Logger.debug("exports is #{inspect(exports)}")
  
    script =
      exports
      |> Enum.reduce("", fn export_map, acc ->
        acc <> export_shell_var(export_map)
      end)
  
    Logger.debug("script is #{inspect(script, limit: :infinity, binaries: :as_strings)}")
  
    {:ok, script, st0}
  end
  
  def export_shell_var(%{"name" => name, "val" => val, "type" => type}) do
    case type do
      "array" ->
        ret = """
        declare -a #{name}
        #{name}=(
        """
  
        ret =
          Melbyd.LuerlUtil.array_to_native_list(val)
          |> Enum.reduce(ret, fn item, acc -> acc <> "#{inspect(item)}" end)
  
        ret <> "\n)\n"
  
      _ ->
        # FIXME: Technically this is broken if "v" has a string "END_HEREDOC" on
        # its own line. There are ways around this but for now we don't care.
        #
        # It's doubly broken because "k" could have an invalid non-keyword
        # character in it. But again we don't care for now.
        """
        #{name}=$(cat << 'END_HEREDOC'
        #{val}
        END_HEREDOC
        )
        """
    end
  end

  def_lua_func read_standard_resource([resource_ref, resource_opts_ref], st0) do
    resource = resource_ref_to_native_map(resource_ref, st0)
    resource_opts = Luerl.decode(st0, resource_opts_ref) |> Melbyd.LuerlUtil.table_to_native_map()
  
    resource_opts = Map.put(resource_opts, "fake", false)
  
    # Now we just have to pass in this data into a SRS initializer function. This
    # initializer function is the Melbyd.StandardResource.get_state() function,
    # which does the work of calling out to the DynamicSupervisor if necessary
    # before retrieving the state. The interesting thing is that the
    # Melbyd.StandardResource module itself calls into the Lua config to determine
    # what kind of business logic it needs to run, especially for the control
    # loop.
    res = Melbyd.StandardResource.read(resource, resource_opts)
    {:ok, res, st0}
  end
  
  def resource_ref_to_native_map(resource_ref, st0) do
    resource_luerl = Luerl.decode(st0, resource_ref)
    resource = Melbyd.LuerlUtil.table_to_native_map(resource_luerl)
    parser = Melbyd.LuerlUtil.table_to_native_map(resource["parser"])
    fake = Melbyd.LuerlUtil.table_to_native_map(resource["fake"])
    resource = Map.put(resource, "parser", parser)
    Map.put(resource, "fake", fake)
  end
  def_lua_func read_standard_resource_fake([resource_ref, resource_opts_ref], st0) do
    resource = resource_ref_to_native_map(resource_ref, st0)
    resource_opts = Luerl.decode(st0, resource_opts_ref) |> Melbyd.LuerlUtil.table_to_native_map()
  
    # Stamp this as being fake for all downstream code.
    resource_opts = Map.put(resource_opts, "fake", true)
  
    # Save vm_fingerprint so that it's accessible easily from the Elixir side.
    {vm_fingerprint, _} = Luerl.get_table1(st0, ["melbyd", "vm_fingerprint"])
    resource_opts = Map.put(resource_opts, "vm_fingerprint", vm_fingerprint)
  
    res = Melbyd.StandardResource.read(resource, resource_opts)
    {:ok, res, st0}
  end
  def_lua_func get_lines_trimmed_nonempty([s], st0) do
    lines =
      s
      |> String.split(@newlines)
      |> Enum.take_while(fn x -> String.trim(x) |> String.length() > 0 end)
  
    {:ok, lines, st0}
  end
  def_lua_func get_trimmed([s], st0) do
    {:ok, String.trim(s), st0}
  end
  def_lua_func split([s, delim], st0) do
    {:ok, String.split(s, delim, trim: true), st0}
  end
  def_lua_func get_group_or_default([s, pat, nth, default], st0) do
    regex =
      try do
        Regex.compile!(pat)
      rescue
        e in Regex.CompileError ->
          Logger.error(got: e, from_pat: pat)
          nil
      end
  
    case regex do
      nil ->
        {:ok, default, st0}
  
      _ ->
        captures = Regex.run(regex, s)
  
        res =
          if captures == nil do
            default
          else
            case Enum.at(captures, nth) do
              nil -> default
              g -> g
            end
          end
  
        {:ok, res, st0}
    end
  end
  
  # Equivalent to get_group_or_default([s, pat, 1, 0], st0)
  def_lua_func get_int_group([s, pat], st0) do
    {:ok, res, _st} = __internal__get_group_or_default([s, pat, 1, "0"], st0)
    {:ok, String.to_integer(res), st0}
  end
  def_lua_func get_lines_matching_count([s, pat], st0) do
    count = String.split(s, @newlines) |> Enum.count(fn x -> String.starts_with?(x, pat) end)
    {:ok, count, st0}
  end
  def_lua_func get_kv_lines_as_map([s], st0) do
    {:ok, lines, _st} = __internal__get_lines_trimmed_nonempty([s], st0)
  
    kvs =
      Enum.map(lines, fn line ->
        [k, v] = String.split(line, ",")
        {k, v}
      end)
  
    {:ok, Map.new(kvs), st0}
  end
  def_lua_func get_columnar_fields_zipped([s, keys_ref], st0) do
    keys = Luerl.decode(st0, keys_ref) |> Melbyd.LuerlUtil.array_to_native_list()
    trimmed_line = String.trim(s)
    vals = String.split(trimmed_line)
    kvs = Enum.zip(keys, vals)
  
    {:ok, Map.new(kvs), st0}
  end
  def_lua_func cast_values([t_ref, keytypes_ref], st0) do
    t = Luerl.decode(st0, t_ref) |> Melbyd.LuerlUtil.table_to_native_map()
    keytypes = Luerl.decode(st0, keytypes_ref) |> Melbyd.LuerlUtil.table_to_native_map()
  
    ret =
      Enum.reduce(t, %{}, fn {key, val}, acc ->
        if Map.has_key?(keytypes, key) do
          case keytypes[key] do
            "float" ->
              Map.put(acc, key, to_number(val, key))
  
            "int" ->
              Map.put(acc, key, Kernel.trunc(to_number(val, key)))
  
            "bool" ->
              Map.put(acc, key, to_bool(val))
  
            # If we can't figure out the type, log an error and use the uncasted value.
            type ->
              Logger.error("key #{inspect(key)}: unrecognized type #{inspect(type)}")
              Map.put(acc, key, val)
          end
        else
          Map.put(acc, key, val)
        end
      end)
  
    {:ok, ret, st0}
  end
  
  def to_number(s, field) when is_binary(s) do
    case Float.parse(s) do
      :error ->
        Logger.error("field #{inspect(field)}: could not convert #{inspect(s)} to float; defaulting to 0")
        0
  
      {f, _rem} ->
        f
      _ -> 0
    end
  end
  
  def to_number(n, field) when is_number(n) do
    Logger.debug("field #{inspect(field)}: #{n} is already a number; using it as-is")
    n
  end
  
  def to_number(x, field) do
    Logger.error("field #{inspect(field)}: #{x} is not a number; defaulting to 0")
    0
  end
  
  def to_bool(s) do
    case s do
      str
      when str in ["", "n", "N", "no", "No", "NO", "nil", "Nil", "NIL", "false", "False", "FALSE"] ->
        false
  
      _ ->
        true
    end
  end
  def_lua_func get_relative_age([unix_seconds_float], st0) do
    Logger.debug("unix_seconds: #{unix_seconds_float}")
    unix_seconds = Kernel.trunc(unix_seconds_float)
    t = Timex.from_unix(unix_seconds)
    {:ok, s} = Timex.format(t, "{relative}", :relative)
  
    {:ok, s, st0}
  end
  def_lua_func get_relative_age_short([unix_seconds_float], st0) do
    Logger.debug("unix_seconds: #{unix_seconds_float}")
    t1 = Kernel.trunc(unix_seconds_float)
    # t1 = Timex.from_unix(unix_seconds)
    t2 = Timex.now() |> Timex.to_unix()
  
    age_seconds = t2 - t1
  
    age = Timex.Duration.from_seconds(age_seconds)
  
    s =
      cond do
        Timex.Duration.to_minutes(age) < 1 ->
          "#{age_seconds}s"
  
        Timex.Duration.to_hours(age) < 1 ->
          minutes = Kernel.trunc(Timex.Duration.to_minutes(age))
          "#{minutes}m"
  
        Timex.Duration.to_hours(age) < 48 ->
          hours = Kernel.trunc(Timex.Duration.to_hours(age))
          "#{hours}h"
  
        Timex.Duration.to_days(age) < 14 ->
          days = Kernel.trunc(Timex.Duration.to_days(age))
          "#{days}d"
  
        Timex.Duration.to_days(age) < 30 ->
          weeks = Kernel.trunc(Timex.Duration.to_days(age) / 7)
          "#{weeks}w"
  
        Timex.Duration.to_days(age) < 365 ->
          months = Kernel.trunc(Timex.Duration.to_days(age) / 30)
          "#{months}M"
  
        true ->
          years = Kernel.trunc(Timex.Duration.to_days(age) / 365)
          "#{years}Y"
      end
  
    {:ok, s, st0}
  end
  def_lua_func get_truncated_personal_moniker([first_last_name, max], st0) do
    Logger.debug("first_last_name: #{first_last_name}")
  
    s =
      case String.split(first_last_name) do
        [] ->
          "?"
  
        [name] ->
          String.slice(name, 0..(max - 1))
  
        [first_name, last_name] ->
          String.first(first_name) <> String.slice(last_name, 0..(max - 2))
  
        names ->
          String.first(List.first(names)) <> String.slice(List.last(names), 0..(max - 2))
      end
  
    {:ok, s, st0}
  end

  def_lua_func broadcast([topic, message_ref], st0) do
    message = Luerl.decode(st0, message_ref) |> Melbyd.LuerlUtil.table_to_native_map_atomic_keys()
  
    payload = Melbyd.LuerlUtil.table_to_native_map_atomic_keys(message.payload)
    payload = Map.put(payload, :time, Calendar.strftime(Timex.local(), "%H:%M:%S"))
    message = Map.put(message, :payload, payload)
  
    Phoenix.PubSub.broadcast(Melbyd.PubSub, topic, message)
  
    # There is nothing to return back to Lua.
    {:ok, nil, st0}
  end
  def_lua_func get_shell_messages([shell_pid, resources_ref, env_vars_ref], st0) do
    resource_luerl_tables =
      Luerl.decode(st0, resources_ref) |> Melbyd.LuerlUtil.array_to_native_list()
    env_vars = Luerl.decode(st0, env_vars_ref) |> Melbyd.LuerlUtil.table_to_native_map()
  
    # Topic handlers is a map where the key is the resource type (e.g.,
    # "srs_Git"), and the handler is the Lua function named "should_keep_message"
    # for that resource.
    topic_handlers =
      resource_luerl_tables
      |> Enum.map(fn resource_luerl_table ->
        resource = Melbyd.LuerlUtil.table_to_native_map(resource_luerl_table)
        {"srs_" <> resource["type"], resource["should_keep_message"]}
      end)
      |> Map.new()
  
    messages = Melbyd.ShellProcess.get_messages(shell_pid, topic_handlers, env_vars)
  
    {:ok, messages, st0}
  end

  def_lua_func log([msg], st0) do
    case msg do
      # If msg is not a primitive (e.g., a string), then decode it first. This way
      # we can handle Lua tables.
      x when is_tuple(x) ->
        y = Luerl.decode(st0, x)
        Logger.debug("(Melbyd Lua SDK debug (string)): #{inspect(y, limit: :infinity, binaries: :as_strings)}")
        Logger.debug("(Melbyd Lua SDK debug (binary)): #{inspect(y, limit: :infinity, binaries: :as_binaries)}")
      x ->
        Logger.debug("(Melbyd Lua SDK debug (string)): #{inspect(x, limit: :infinity, binaries: :as_strings)}")
        Logger.debug("(Melbyd Lua SDK debug (binary)): #{inspect(x, limit: :infinity, binaries: :as_binaries)}")
    end
    {:ok, nil, st0}
  end
end
defmodule Melbyd.LuerlUtil do
  def table_lookup(_t, []) do
    raise ArgumentError, message: "lookup_keys cannot be empty"
  end
  
  def table_lookup(t, lookup_keys) do
    Enum.reduce(lookup_keys, t, fn lookup_key, val_so_far ->
      if not Kernel.is_list(val_so_far) do
        raise ArgumentError, message: "val_so_far '#{inspect(val_so_far)}' is not a list"
      end
  
      Enum.map(val_so_far, &verify_table_tuple/1)
  
      found = Enum.find(val_so_far, fn {t_key, _t_val} -> t_key == lookup_key end)
  
      if found == nil do
        raise ArgumentError, message: "lookup_key '#{lookup_key}' not found"
      else
        {_t_key, t_val} = found
        t_val
      end
    end)
  end
  
  def verify_table_tuple({k, _v}) when is_binary(k) do
    :ok
  end
  
  def verify_table_tuple(x) do
    raise ArgumentError, message: "element '#{inspect(x)}' is not a well-formed tuple"
  end
  def array_to_native_list(a) do
    Enum.map(a, &verify_array_tuple/1)
  
    {_, native_list} =
      Enum.reduce(a, {0, []}, fn {k, v}, {i, native_list} ->
        if k == i + 1 do
          {k, [v | native_list]}
        else
          raise ArgumentError, message: "expected index #{i + 1} for element '#{inspect({k, v})}'"
        end
      end)
  
    Enum.reverse(native_list)
  end
  
  def verify_array_tuple({k, _v}) when is_integer(k) do
    :ok
  end
  
  def verify_array_tuple(x) do
    raise ArgumentError, message: "element '#{inspect(x)}' is not a well-formed tuple"
  end
  def table_to_native_map(t) do
    Enum.map(t, &verify_table_tuple/1)
    Map.new(t)
  end
  def table_to_native_map_atomic_keys(t) do
    Enum.map(t, &verify_table_tuple/1)
    Enum.map(t, fn {k, v} -> {String.to_atom(k), v} end) |> Map.new()
  end
end
