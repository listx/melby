# Copyright 2024 Linus Arver
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

defmodule Melbyd.ShellProcess do
  use GenServer, restart: :temporary
  require Logger

  @tick_interval 1000

  def start_link(
        %{
          shell_pid: shell_pid,
          topic_handlers: _topic_handlers,
          env_vars: _env_vars,
          ttl: _ttl,
          notify_on_exit_pid: _notify_on_exit_pid
        } = args_for_init
      ) do
    GenServer.start_link(__MODULE__, args_for_init, name: via_tuple(shell_pid))
  end

  defp via_tuple(shell_pid) do
    key = {:n, :l, {__MODULE__, shell_pid}}
    {:via, :gproc, key}
  end

  @impl true
  def init(
        %{
          shell_pid: shell_pid,
          topic_handlers: topic_handlers,
          env_vars: env_vars,
          ttl: ttl,
          notify_on_exit_pid: notify_on_exit_pid
        } = _args
      ) do
    Process.flag(:trap_exit, true)
    Logger.info("Starting SPS #{inspect(shell_pid)}; ttl=#{inspect(ttl)}")

    # Subscribe to topics.
    topics = Enum.map(topic_handlers, fn {topic, _handler} -> topic end)
    Enum.map(topics, fn topic -> Phoenix.PubSub.subscribe(Melbyd.PubSub, topic) end)
    Logger.info("Subscribed to these topics: #{inspect(topics)}")

    initial_state = %{
      shell_pid: shell_pid,
      messages: [],
      topic_handlers: topic_handlers,
      topics: topics,
      env_vars: env_vars,
      ttl: ttl,
      ttl_max: ttl,
      notify_on_exit_pid: notify_on_exit_pid
    }

    # Start up the tick process to detect TTL deadlines.
    tick(ttl, notify_on_exit_pid)

    {:ok, initial_state}
  end

  @impl true
  def handle_call(
        {:get_messages, topic_handlers},
        _from,
        %{shell_pid: shell_pid,
          messages: messages,
          topics: already_subscribed_topics, ttl_max: ttl_max} = state
      ) do

    # If there are topics of interest that have not yet been subscribed to,
    # subscribe to them as well. But also unsubscribe from topics that we don't
    # care about any more.
    #
    # FIXME: In practice, because our Lua config is essentially immutable, we
    # never unsubscribe from topics because the topics list never changes.
    topics = Enum.map(topic_handlers, fn {topic, _handler} -> topic end)
    topics_new = topics -- already_subscribed_topics
    Enum.map(topics_new, fn topic -> Phoenix.PubSub.subscribe(Melbyd.PubSub, topic) end)
    topics_obsolete = already_subscribed_topics -- topics
    Enum.map(topics_obsolete, fn topic -> Phoenix.PubSub.unsubscribe(Melbyd.PubSub, topic) end)

    if length(messages) > 0 do
      Logger.info("SPS #{shell_pid}: sending messages to client: #{inspect(messages)}")
    end

    {
      :reply,
      # Reverse the messages, because we store the newest one first.
      Enum.reverse(messages),
      # Erase messages buffer because we've just dumped it to the client.
      %{state | messages: [], ttl: ttl_max}
    }
  end

  @impl true
  def handle_call(
        {:update_env_vars, env_vars_new},
        _from,
        %{shell_pid: shell_pid, env_vars: env_vars_old} = state
      ) do

    if env_vars_new != env_vars_old do
      Logger.info("SPS #{shell_pid}: updating env_vars from #{inspect(env_vars_old)} " <>
        "to #{inspect(env_vars_new)}")
    end

    {
      :reply,
      nil,
      %{state | env_vars: env_vars_new}
    }
  end

  # GenServer callbacks.
  @impl true
  def handle_info(
        :tick,
        %{
          ttl: ttl,
          notify_on_exit_pid: notify_on_exit_pid
        } = state
      ) do
    # Continue ticking for the future. But optionally die if ttl is too low.
    tick(ttl, notify_on_exit_pid)
    {:noreply, %{state | ttl: ttl - 1}}
  end
  @impl true
  def handle_info(
        %{topic: topic, from: _from, payload: _payload} = message,
        %{
          shell_pid: shell_pid,
          messages: messages,
          topic_handlers: topic_handlers,
          env_vars: env_vars
        } = state
      ) do
    Logger.info("SPS #{shell_pid}: Handling PubSub message: #{inspect(message)}")
  
    keep_message =
      if Map.has_key?(topic_handlers, topic) do
        should_keep_message = topic_handlers[topic]
        should_keep_message.([message, env_vars]) |> Kernel.hd()
      else
        # If we can't find an associated filter function for this topic, discard it
        # but log a warning.
        Logger.warning("could not find filter function for PubSub message #{inspect(message)}")
  
        false
      end
  
    if keep_message do
      Logger.info("SPS #{shell_pid}: Keeping message #{inspect(message)}")
      {:noreply, %{state | messages: [message | messages]}}
    else
      Logger.info("SPS #{shell_pid}: Dropping message #{inspect(message)}")
      {:noreply, state}
    end
  end
  @impl true
  def handle_info(
        {:EXIT, from_pid, reason},
        %{shell_pid: shell_pid} = state
      ) do
    Logger.debug("SPS #{shell_pid}: Got exit reason #{inspect(reason)} from pid #{inspect(from_pid)}; exiting")
    case reason do
      :normal ->
        # This can happen if, e.g., a System.cmd/3 finishes running successfully.
        {:noreply, state}
      :ttl_deadline_exceeded ->
        # Invoke our terminate/2 callback by returning with the ":stop" atom.
        {:stop, :normal, state}
      _ ->
        {:stop, reason, state}
    end
  end
  
  @impl true
  def terminate(
        reason,
        %{shell_pid: shell_pid} = _state
      ) do
    Logger.info("SPS #{shell_pid}: Got exit reason #{inspect(reason)}; shutting down")
  end

  # Tick
  # FIXME: This code is identical to the one in Melbyd.StandardResource.tick/2. Can
  # we make it DRY somehow?
  defp tick(ttl, notify_on_exit_pid) do
    case ttl do
      n when n in -1..0 ->
        if n == 0 do
          Logger.info("TTL expired; shutting down this GenServer due to client neglect")
        else
          Logger.info(
            "TTL expired manually; shutting down this GenServer"
          )
        end
  
        # Used for testing, where we assert that we can receive this
        # ":shutting_down" message after the ttl expires.
        if notify_on_exit_pid do
          send(notify_on_exit_pid, :shutting_down)
        end
  
        Process.exit(self(), :ttl_deadline_exceeded)
      _ ->
        Process.send_after(self(), :tick, @tick_interval)
    end
  end

  # Client interface
  def get_messages(shell_pid, topic_handlers, env_vars) do
    case :gproc.lookup_pids({:n, :l, {Melbyd.ShellProcess, shell_pid}}) do
      [pid] ->
        Logger.info("Found existing pid for #{inspect(shell_pid)}: #{inspect(pid)}")
        # We update the env_vars in the GenServer state, because otherwise it will
        # always keep the same env_vars that it was created with.
        GenServer.call(pid, {:update_env_vars, env_vars})
        GenServer.call(pid, {:get_messages, topic_handlers})
  
      _ ->
        Task.Supervisor.start_child(Melbyd.TaskSupervisor, fn ->
          Melbyd.ShellProcessSupervisor.start_sps(shell_pid, topic_handlers, env_vars)
        end)
  
        # Return empty list (no messages) for now.
        []
    end
  end
end


defmodule Melbyd.ShellProcessSupervisor do
  use DynamicSupervisor

  require Logger

  def start_link(init_arg) do
    Logger.info("Starting SPS dynamic supervisor")
    DynamicSupervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_sps(shell_pid, topic_handlers, env_vars) do
    case start_child(shell_pid, topic_handlers, env_vars) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
      unknown -> Logger.warning("start_sps failed: #{inspect(unknown)}")
    end
  end

  defp start_child(shell_pid, topic_handlers, env_vars) do
    ttl = Application.get_env(:melbyd, :melbyd_sps_ttl)

    DynamicSupervisor.start_child(
      __MODULE__,
      {Melbyd.ShellProcess, %{shell_pid: shell_pid,
                             topic_handlers: topic_handlers,
                             env_vars: env_vars,
                             ttl: ttl,
                             notify_on_exit_pid: nil}}
    )
  end
end
