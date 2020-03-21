declare -a g_cars g_cdrs

make_pair() {
    local id=${#g_cars[@]}
    g_cars[$id]=$1
    g_cdrs[$id]=$2
    g_return="%[${id}]"
}

unwrap_generic() {
    local value=$1
    value="${value:2}"
    value="${value%]}"
    g_return="$value"
}

unwrap_integer() {
    local value=$1
    if [[ "$value" != "#"* ]]; then
        die "not an integer"
    fi
    unwrap_generic "$value"
}

unwrap_symbol() {
    local value=$1
    if [[ "$value" != "&"* ]]; then
        die "not a symbol"
    fi
    unwrap_generic "$value"
}

unwrap_pair() {
    local value=$1
    if [[ "$value" != "%"* ]]; then
        die "not a pair"
    fi
    unwrap_generic "$value"
}

unwrap_func() {
    local value=$1
    if [[ "$value" != "@"* ]]; then
        die "not a function"
    fi
    unwrap_generic "$value"
}

stringify() {
    local value=$1
    local id tmp
    case "$value" in
    _) g_return="#undef";;
    T) g_return="#t";;
    F) g_return="#f";;
    '$') g_return="()";;
    "#"*|"&"*)
        unwrap_generic "$value"
        ;;
    "@"*)
        unwrap_generic "$value"
        g_return="${g_return%% *}"
        ;;
    "%"*)
        unwrap_pair "$value"
        id="$g_return"
        stringify "${g_cars[$id]}"
        tmp="$g_return"
        stringify "${g_cdrs[$id]}"
        g_return="(${tmp} . ${g_return})"
        ;;
    *)
        die "stringify: corrupted value: $value"
        ;;
    esac
}

array_to_value() {
    local value='$'
    local i v
    for (( i = $#; i >= 1; i-- )); do
        v="${!i}"
        make_pair "$v" "$value"
        value="$g_return"
    done
    g_return="$value"
}

value_to_array() {
    local value=$1
    local array=()
    while [[ "$value" == '%'* ]]; do
        unwrap_pair "$value"
        array+=("${g_cars[$g_return]}")
        value="${g_cdrs[$g_return]}"
    done
    if [[ "$value" != '$' ]]; then
        die "not a list: $1"
    fi
    g_return=("${array[@]}")
}
