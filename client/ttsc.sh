#!/bin/bash
#

[[ -x $(which jq) ]] || (echo 'Please install "jq"' && exit 2)

URI_SCHEME='http'
# let curl be 
# silent (-s)
# write the http_code (-w)
CURL_OPTS='-s -w %{http_code}'

function perform-request {
    method=$1
    host=$2
    url_path=$3
    params=$4

    url=$URI_SCHEME://$host/$url_path?$params
    
    resp=$(curl $CURL_OPTS -X $method $url)
    
    length=${#resp}
    http_status=${resp:$length-3:3} 
    http_status_first=${resp:$length-3:1} 
    body=${resp:0:$length-3}
    echo $body

    if [[ ! $http_status_first = "2" ]]
    then
	>&2 echo ERROR RESPONSE $http_status
	exit 5
    fi

}

function list-endservices {
    host=$1
    at=$2
    op=$3
    mode=$4

    if [[ -n $at ]]
    then
       [[ -z $op ]] && echo 'Missing OP' && exit 1
       [[ $mode = 'active' || $mode = 'inactive' ]] || (echo 'Invalid mode: ' $mode && exit 1)

	args="access_token=$at&provider_id=$op&list_mode=$mode"
    fi

    resp=$(perform-request GET $host api/endservice/list $args)

    [[ $? -ne 0 ]] && exit $?

    services=$(echo $resp | jq '.endservices')

    if [[ $? -ne 0 ]]
    then
	>&2 echo Something other than JSON returned from server
    fi

    ids=($(echo $services | jq -c '.[].id'))
    names=($(echo $services | jq -c '.[] .name'))
    hosts=($(echo $services | jq -c '.[] .host'))

    if [[ $? -ne 0 ]]
    then
       echo $resp
       exit 3
    fi


    # echo "id  name   host"
    for ((i=0; i<${#ids[@]}; i++))
    do
	echo ${ids[$i]} ${names[$i]} ${hosts[$i]}
    done
}

function list-providers {
    host=$1

    json=$(perform-request GET $host api/oidc/provider/list)

    [[ $? -ne 0 ]] && exit $?

    provider=$(echo $json | jq '.oidc_provider') 2>/dev/null

    if [[ $? -ne 0 ]]
    then
	>&2 echo Something other than JSON returned from server
    fi


    ids=($(echo $provider | jq '.[] .id'))
    issuer=($(echo $provider | jq '.[] .iss'))

    # echo "id  issuer"
    for ((i=0; i<${#ids[@]}; i++))
    do
	echo ${ids[$i]} ${issuer[$i]}
    done
}

function request-endservice-access {
    host=$1
    at=$2
    pid=$3
    sid=$4

    perform-request POST $host api/endservice/access \
	       "access_token=$at&provider_id=$pid&service_id=$sid"
}

function revoke-endservice-access {
    host=$1
    at=$2
    pid=$3
    sid=$4

    perform-request DELETE $host api/endservice/access \
	       "access_token=$at&provider_id=$pid&service_id=$sid"
}

name=$0
cmd=$1
shift

case $cmd in
    lsprov)
	list-providers $@
	;;
    lsserv)
	list-endservices $@
	;;
    request)
	request-endservice-access $@
	;;
    revoke)
	revoke-endservice-access $@
	;;
    func)
    perform-request $@
    ;;
    --help|-h|help)
	echo 'Usage:'
	# echo '    lsprov <host> [<ac_token> <provider_id> <mode>]     # List all providers'
	echo '    lsserv <host>                                       # List all end-services'
	echo '    request <host> <ac_token> <provider_id> <service_id> # Request access'
	echo '    revoke <host> <ac_token> <provider_id> <service_id> # Revoke access'
	;;
    *)
	echo 'Unknown command'
	echo "Try $0 --help|-h|help"
	exit 1
	;;
esac

