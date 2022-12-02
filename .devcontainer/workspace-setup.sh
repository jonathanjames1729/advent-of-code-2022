#!/bin/bash --login

on_create() {
    local -r workspace_path="$1"
}

on_start() {
    local -r workspace_path="$1"

    pushd "$workspace_path"
    git config --global --unset-all safe.directory "$workspace_path"
    git config --global --add safe.directory "$workspace_path"
    popd
}

on_attach() {
    local -r workspace_path="$1"
}

main() {
    local -r stage="$1"
    local -r workspace_path="$2"

    case "$stage" in
    'CREATE')
      on_create "$workspace_path"
      ;;
    'START')
      on_start "$workspace_path"
      ;;
    'ATTACH')
      on_attach "$workspace_path"
      ;;
    esac
}

main "$@"
