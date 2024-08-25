use envsubst;
use std::collections::HashMap;

#[cfg_attr(feature = "elixir_support", rustler::nif)]
pub fn path_shorten(
    path: &str,
    aliases: HashMap<String, String>,
    env_vars: HashMap<String, String>,
    shorten_threshold: u8,
) -> String {
    let path_canonical = make_canonical_path(path, &aliases, &env_vars);
    _path_shorten(&path_canonical, shorten_threshold)
}

fn _path_shorten(path_canonical: &str, shorten_threshold: u8) -> String {
    // Don't shorten if the shorten_threshold is disabled.
    if shorten_threshold == 0 {
        return path_canonical.to_string();
    }

    // Don't shorten paths that are `shorten_threshold` characters or less in
    // length.
    if path_canonical.chars().count() <= shorten_threshold.into() {
        return path_canonical.to_string();
    }

    // Don't bother shortening anything if there is only 1 directory.
    let parts_count = path_canonical.split("/").count();
    if parts_count == 1 {
        return path_canonical.to_string();
    }
    let first_char = path_canonical.chars().next().unwrap();
    if first_char == '/' && parts_count == 2 {
        return path_canonical.to_string();
    }

    // Determine overall "search" area of possible directories within the path
    // to shorten to 1 character. We exclude from the search the very first and
    // last directories.
    let (j, shortenable_dirs) = match first_char {
        // Do not shorten leading directories that start with '~', and also do
        // not consider the root directory '/'.
        '/' | '~' => (1, 1..(parts_count - 1)),
        _ => (0, 0..(parts_count - 1)),
    };

    // Construct a set of ranges, using shortenable_dirs. E.g., if
    // shortenable_dirs is (1..3), then construct:
    //   (1..2)
    //   (1..3)
    //   (1..4).
    // We use these ranges to denote directories that should be shortened. As
    // these ranges include more and more numbers, we shorten more and more
    // directories until we are satisified with how much we've shortened
    // path_canonical.
    let mut ranges: Vec<std::ops::Range<usize>> = Vec::new();

    for i in shortenable_dirs {
        ranges.push(j..i + 1);
    }

    let mut candidate_best: Option<String> = None;
    for range in ranges {
        // Construct shortened path candidate with all directories in the range
        // shortened.
        let mut candidate: Vec<String> = Vec::new();
        for (part_idx, part) in path_canonical.split("/").enumerate() {
            if range.contains(&part_idx) {
                // Add shortened version.
                candidate.push(part.chars().next().unwrap().to_string());
            } else {
                // Add as-is.
                candidate.push(part.to_string());
            }
        }
        let shortened = candidate.join("/");
        // If a better (shorter) candidate is found, prefer it over the previous
        // candidate.
        if candidate_best.is_none()
            || shortened.chars().count() < candidate_best.as_ref().unwrap().chars().count()
        {
            candidate_best = Some(shortened);
        };

        // If a candidate is already under 30 characters, stop searching.
        if candidate_best.is_some() && candidate_best.as_ref().unwrap().chars().count() <= 30 {
            break;
        }
    }

    if candidate_best.is_none() {
        path_canonical.to_string()
    } else {
        candidate_best.unwrap().to_string()
    }
}

fn make_canonical_path(
    path: &str,
    aliases: &HashMap<String, String>,
    env_vars: &HashMap<String, String>,
) -> String {
    // For every aliased path, replace all matching environment variable
    // references in the path with their actual runtime values. For example, if
    // aliases has an entry like "${HOME}/foo/bar" and "${HOME}" is set to
    // "/home/alice", then replace the name entry with
    // "/home/alice/foo/bar".
    let mut aliases_expanded: HashMap<String, String> = HashMap::new();

    // Remove env vars that have invalid values, because otherwise the envsubst
    // library chokes (even if we aren't trying to use the invalid values).
    let mut env_vars_cleaned: HashMap<String, String> = HashMap::new();
    for (k, v) in env_vars {
        let mut context: HashMap<String, String> = HashMap::new();
        context.insert(k.to_string(), v.to_string());
        if envsubst::validate_vars(&context).is_ok() {
            env_vars_cleaned.insert(k.to_string(), v.to_string());
        }
    }

    for (path_maybe_has_env_vars, name) in aliases.into_iter() {
        let expanded_path =
            envsubst::substitute(path_maybe_has_env_vars, &env_vars_cleaned).unwrap();
        aliases_expanded.insert(expanded_path, name.to_string());
    }

    // Encode the shell's "~" character as a special case of our "name" idiom.
    // This way, even if no aliases match, we can replace "/home/foo" with "~".
    // FIXME: This requires HOME to be set. Otherwise we'll panic when we
    // unwrap() below.
    let home_value = env_vars.get(&"HOME".to_string()).unwrap();
    // Don't replace $HOME with "~" becausewe we already prepend every name
    // match with a "~". So defining the value here with a tilde would result in
    // a "~~".
    aliases_expanded.insert(home_value.to_string(), "".to_string());

    let path_canonical = match get_matching_name(path, &aliases_expanded) {
        // Find the longest matching expanded path in the path aliases. If there
        // is a match, then we use "~ALIAS" (the leading "~" does not mean $HOME
        // and just signifies that the word that immediately follows it is a
        // path alias). This is a Zsh-ism.
        Some((expanded_path, name)) => {
            let aliased_path = path.replacen(&expanded_path, &name, 1);
            format!("~{}", aliased_path)
        }
        // If there is no match, return the path as-is.
        None => path.to_string(),
    };

    path_canonical
}

fn get_matching_name(path: &str, aliases: &HashMap<String, String>) -> Option<(String, String)> {
    let mut aliased_paths = aliases.keys().collect::<Vec<_>>();
    aliased_paths.sort();
    aliased_paths.reverse();

    for aliased_path in aliased_paths {
        if path.starts_with(aliased_path) {
            let name = aliases.get(aliased_path).unwrap();
            return Some((aliased_path.to_string(), name.to_string()));
        }
    }

    None
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_make_canonical_path() {
        let mut aliases: HashMap<String, String> = HashMap::new();
        aliases.insert("${HOME}/bar".to_string(), "b".to_string());
        aliases.insert(
            "${HOME}/bar/baz/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx/c".to_string(),
            "c".to_string(),
        );
        aliases.insert("${MYPROJECT_DIR}".to_string(), "p".to_string());
        // Handle aliases composed of multiple environment variables.
        aliases.insert(
            "${MYPROJECT_DIR}/${KOALA_SIZE}".to_string(),
            "pk".to_string(),
        );

        let mut env_vars: HashMap<String, String> = HashMap::new();
        env_vars.insert("HOME".to_string(), "/home/foo".to_string());
        env_vars.insert(
            "MYPROJECT_DIR".to_string(),
            "/home/foo/myproject".to_string(),
        );
        env_vars.insert("KOALA_SIZE".to_string(), "big".to_string());

        assert_eq!(make_canonical_path("", &aliases, &env_vars), "");
        assert_eq!(make_canonical_path("/", &aliases, &env_vars), "/");
        assert_eq!(
            make_canonical_path("/unrecognized/path", &aliases, &env_vars),
            "/unrecognized/path"
        );
        assert_eq!(make_canonical_path("/home/foo", &aliases, &env_vars), "~");
        assert_eq!(
            make_canonical_path("/home/foo/bar", &aliases, &env_vars),
            "~b"
        );
        assert_eq!(
            make_canonical_path(
                "/home/foo/bar/baz/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx/c",
                &aliases,
                &env_vars,
            ),
            "~c"
        );
        assert_eq!(
            make_canonical_path("/home/foo/myproject", &aliases, &env_vars),
            "~p"
        );
        assert_eq!(
            make_canonical_path("/home/foo/myproject/big", &aliases, &env_vars),
            "~pk"
        );
    }

    #[test]
    fn test_path_shorten() {
        assert_eq!(_path_shorten("", 30), "");
        assert_eq!(_path_shorten("~", 30), "~");
        assert_eq!(_path_shorten("/", 30), "/");
        assert_eq!(_path_shorten("/a", 30), "/a");
        assert_eq!(_path_shorten("/a/b/c", 30), "/a/b/c");
        assert_eq!(_path_shorten("a", 30), "a");
        assert_eq!(_path_shorten("a/b/c", 30), "a/b/c");
        // If the path is exactly 30 characters, we should not shorten anything.
        assert_eq!(
            _path_shorten("/a23456789/b23456789/c23456789", 30),
            "/a23456789/b23456789/c23456789"
        );
        // If the path is just over 30 characters, we should shorten the first
        // directory.
        assert_eq!(
            _path_shorten("/a23456789/b23456789/c23456789d", 30),
            "/a/b23456789/c23456789d"
        );
        // Some longer directories.
        assert_eq!(
            _path_shorten("/a23456789/b23456789/c23456789/d23456789", 30),
            "/a/b/c23456789/d23456789"
        );
        assert_eq!(
            _path_shorten("a23456789/b23456789/c23456789/d23456789", 30),
            "a/b/c23456789/d23456789"
        );
        // Shortening of aliases (directories with "~") in them are forbidden.
        assert_eq!(
            _path_shorten("~a23456789/b23456789/c23456789/d23456789", 30),
            "~a23456789/b/c/d23456789"
        );
        // Realistic example (last directory remains untouched).
        assert_eq!(
            _path_shorten("~/prog/foreign/git/contrib/thunderbird-patch-inline", 30),
            "~/p/f/g/c/thunderbird-patch-inline"
        );
        // Extreme cases.
        assert_eq!(
            _path_shorten(
                "~/aaaaaaaaaaaaaaaaaaaa/bbbbbbbbbbbbbbbbbbbbbb/cccccccccccccccccccccc/hello",
                30
            ),
            "~/a/b/c/hello"
        );
        // Unusual case of just 2 directories, where both are very long.
        assert_eq!(
            _path_shorten(
                "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
                30
            ),
            "a/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
        );
        // Non-ASCII (exactly 30 characters).
        assert_eq!(
            _path_shorten(
                "/일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구",
                30
            ),
            "/일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구"
        );
        assert_eq!(
            _path_shorten(
                "일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구a",
                30
            ),
            "일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구a"
        );
        assert_eq!(
            _path_shorten(
                "~일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구",
                30
            ),
            "~일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구"
        );
        // Non-ASCII (over 30 characters).
        assert_eq!(
            _path_shorten(
                "/일이삼사오육칠팔구/일이삼사오육칠팔구/일이삼사오육칠팔구/a",
                30
            ),
            "/일/일이삼사오육칠팔구/일이삼사오육칠팔구/a"
        );
        let longstr = concat!(
            "~/일일일일일일일일일일일일일일일일일일일일",
            "/이이이이이이이이이이이이이이이이이이이이",
            "/삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼삼",
            "/hello"
        );
        assert_eq!(_path_shorten(longstr, 30), "~/일/이/삼/hello");
        // Shorten threshold is disabled, so don't shorten at all.
        assert_eq!(_path_shorten(longstr, 0), longstr);
    }
}
