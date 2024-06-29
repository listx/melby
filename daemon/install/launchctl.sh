#!/usr/bin/env bash
#
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

set -euo pipefail

SCRIPT_ROOT="$(dirname "$(realpath "$0")")"

pushd "${HOME}/Library/LaunchAgents"

filename=com.melby.daemon.plist

launchctl unload "${filename}" || true

cat <<EOF > "${filename}"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
                "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>

    <key>Label</key>
    <string>${filename}</string>

    <key>EnvironmentVariables</key>
    <dict>
      <key>PATH</key>
      <string><![CDATA[${PATH}]]></string>
    </dict>

    <key>Program</key>
    <string>${SCRIPT_ROOT}/melbyd</string>

    <key>KeepAlive</key>
    <true/>

  </dict>
</plist>

EOF

launchctl load "${filename}"
