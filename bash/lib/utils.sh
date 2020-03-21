declare -r DEBUG=false

die() {
    echo "$@" >&2
    exit 1
}
