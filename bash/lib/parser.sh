skip() {
    local c
    while :; do
        c="${g_code:0:1}"
        if [[ -z "$c" ]]; then
            break
        fi
        if [[ "$c" != ' ' && "$c" != $'\n' ]]; then
            break
        fi
        g_code="${g_code:1}"
    done
}

parse_partial_value() {
    local value values token i j c
    if [[ "$g_code" == \'* ]]; then
        g_code=${g_code:1}
        parse_partial_value
        make_pair "$g_return" '$'
        make_pair '&[quote]' "$g_return"
        return
    fi

    if [[ "$g_code" == '('* ]]; then
        g_code=${g_code:1}
        parse_partial_list
        skip
        if [[ "$g_code" != ')'* ]]; then
            die "unexpected end of list"
        fi
        g_code=${g_code:1}
        array_to_value "${g_return[@]}"
        return
    fi

    token=
    i=0
    while :; do
        c=${g_code:$i:1}
        if [[ "$c" == '' || "$c" == ' ' || "$c" == $'\n' || "$c" == ')' || "$c" == ';' ]]; then
            break
        fi
        i=$((i + 1))
    done
    token=${g_code:0:$i}
    g_code=${g_code:$i}
    if [[ "$token" =~ ^-?[0-9]+$ ]]; then
        g_return="#[${token}]"
    else
        case "$token" in
        '#f') g_return='F';;
        '#t') g_return='T';;
        *) g_return="&[${token}]"
        esac
    fi
}

parse_partial_list() {
    local value
    local values=()
    while :; do
        skip
        if [[ -z "$g_code" || "$g_code" == ')'* ]]; then
            break
        fi
        parse_partial_value
        values+=("$g_return")
    done
    g_return=("${values[@]}")
}

parse_list() {
    g_code="$1"
    parse_partial_list
    skip
    if [[ -n "$g_code" ]]; then
        die "excess parentheses"
    fi
}

parse_file() {
    parse_list "$(cat -- "$1")"
}
