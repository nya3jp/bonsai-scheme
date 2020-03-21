g_env_next_id=1

new_env() {
    local parent=$1
    local env="env${g_env_next_id}"
    : $(( g_env_next_id++ ))
    eval "${env}_parent='$parent'"
    eval "${env}_vars=()"
    g_return="$env"
}

put_env() {
    local env=$1
    local key=$2
    local value=$3
    local vars_size
    eval "vars_size=\${#${env}_vars[@]}"
    local tmp i akey avalue
    for (( i = 0; i < vars_size; i++ )); do
        tmp="${env}_vars[${i}]"
        IFS=' ' read -r akey avalue <<< "${!tmp}"
        if [[ "$akey" == "$key" ]]; then
            if $DEBUG; then
                stringify "$value"
                echo "DEBUG: put_env: ${env}: $key = $g_return (overwrite)" >&2
            fi
            eval "${env}_vars[${i}]=\"\$key \$value\""
            return
        fi
    done
    if $DEBUG; then
        stringify "$value"
        echo "DEBUG: put_env: ${env}: $key = $g_return (new)" >&2
    fi
    eval "${env}_vars+=(\"\$key \$value\")"
}

set_env() {
    local env=$1
    local key=$2
    local value=$3
    local vars_size
    local tmp i akey avalue
    while [[ -n "$env" ]]; do
        eval "vars_size=\${#${env}_vars[@]}"
        for (( i = 0; i < vars_size; i++ )); do
            tmp="${env}_vars[${i}]"
            IFS=' ' read -r akey avalue <<< "${!tmp}"
            if [[ "$akey" == "$key" ]]; then
                if $DEBUG; then
                    stringify "$value"
                    echo "DEBUG: set_env: ${env}: $key = $g_return (overwrite)" >&2
                fi
                eval "${env}_vars[${i}]=\"\$key \$value\""
                return
            fi
        done
        tmp="${env}_parent"
        env="${!tmp}"
    done
    die "set_env: name not found: $key"
}

get_env() {
    local env=$1
    local key=$2
    local tmp entry akey avalue
    while [[ -n "$env" ]]; do
        tmp="${env}_vars[@]"
        for entry in "${!tmp}"; do
            IFS=' ' read -r akey avalue <<< "$entry"
            if [[ "$akey" == "$key" ]]; then
                g_return="$avalue"
                return
            fi
        done
        tmp="${env}_parent"
        env="${!tmp}"
    done
    die "get_env: name not found: $key"
}
