# Instead of printing logs, capture them. Only print them for tests that fail.
# See https://hashrocket.com/blog/posts/silence-logger-messages-in-exunit.
ExUnit.start(capture_log: true)
