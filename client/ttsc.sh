#!/bin/bash
#

[[ -x $(which jq) ]] || (echo 'Please install "jq"' && exit 2)

[[ -z $URI_SCHEME ]] && URI_SCHEME='https'

# let curl be
# silent (-s)
# write the http_code (-w)
CURL_OPTS='-s -w %{http_code}%{redirect_url} --insecure '

function perform-post {
    url=$1
    ac_token=$2
    body=$3
    iss=$4
    if [[ $iss = '' ]]; then
        resp=$(curl $CURL_OPTS -H "Content-Type: application/json" \
            --data-raw "$body" -H "Authorization: Bearer $ac_token" \
            -X POST $url)
    else
        resp=$(curl $CURL_OPTS -H "Content-Type: application/json" \
            --data-raw "$body" -H "Authorization: Bearer $ac_token" \
            -H "X-OpenId-Connect-Issuer: $iss" \
            -X POST $url)
    fi
    length=${#resp}
    http_status=${resp:0:3}
    redirect=${resp:3:$length-3}

    if [[ $http_status = "303" ]]
    then
        perform-get $redirect $ac_token $iss
    elif [[ ! $http_status_first = "2" ]]
    then
        >&2 echo ERROR RESPONSE $http_status
        exit 5
    else
        echo $body
    fi
}

function perform-get {
    url=$1
    ac_token=$2
    iss=$3
    if [[ $iss = '' ]]; then
        resp=$(curl $CURL_OPTS -H "Authorization: Bearer $ac_token" \
            -X GET $url)
    else
        resp=$(curl $CURL_OPTS -H "Authorization: Bearer $ac_token" \
            -H "X-OpenId-Connect-Issuer: $iss" \
            -X GET $url)
    fi
    length=${#resp}
    http_status=${resp:$length-3:3}
    body=${resp:0:$length-3}

    http_status_first=${resp:$length-3:1}

    echo $body

    if [[ ! $http_status_first = "2" ]]
    then
        >&2 echo ERROR RESPONSE $http_status
        exit 5
    fi
}

function perform-delete {
    url=$1
    ac_token=$2
    iss=$3
    if [[ $iss = '' ]]; then
        resp=$(curl $CURL_OPTS -H "Authorization: Bearer $ac_token" \
            -X DELETE $url)
    else
        resp=$(curl $CURL_OPTS -H "Authorization: Bearer $ac_token" \
            -H "X-OpenId-Connect-Issuer: $iss" \
            -X DELETE $url)
    fi
    length=${#resp}
    http_status=${resp:$length-3:3}
    body=${resp:0:$length-3}

    http_status_first=${resp:$length-3:1}

    echo $body

    if [[ ! $http_status_first = "2" ]]
    then
        >&2 echo ERROR RESPONSE $http_status
        exit 5
    fi
}
function list-endservices {
    host=$1
    ac_token=$2
    iss=$3
    url_path=api/service
    url=$URI_SCHEME://$host/$url_path
    resp=$(perform-get $url $ac_token $iss)

    [[ $? -ne 0 ]] && exit $?

    services=$(echo $resp | jq '.service_list')

    if [[ $? -ne 0 ]]
    then
	>&2 echo Something other than JSON returned from server
    fi

    ids=($(echo $services | jq -c '.[].id'))
    types=($(echo $services | jq -c '.[].type'))
    ports=($(echo $services | jq -c '.[].port'))
    hosts=($(echo $services | jq -c '.[].host'))

    if [[ $? -ne 0 ]]
    then
       echo $resp
       exit 3
    fi


    #echo "id  type   host  port"
    for ((i=0; i<${#ids[@]}; i++))
    do
	echo ${ids[$i]} ${types[$i]} ${hosts[$i]} ${ports[$i]}
    done
}

function list-credentials {
    host=$1
    ac_token=$2
    iss=$3
    url_path=api/credential
    url=$URI_SCHEME://$host/$url_path
    resp=$(perform-get $url $ac_token $iss)

    [[ $? -ne 0 ]] && exit $?

    credentials=$(echo $resp | jq -c '.credential_list')

    if [[ $? -ne 0 ]]
    then
	>&2 echo Something other than JSON returned from server
    fi

    ids=($(echo $credentials | jq -c '.[].id'))

    if [[ $? -ne 0 ]]
    then
       echo $resp
       exit 3
    fi

    #echo "id  type   host  port"
    for ((i=0; i<${#ids[@]}; i++))
    do
	echo ${ids[$i]}
    done
}
function list-providers {
    host=$1
    url_path=api/oidcp
    url=$URI_SCHEME://$host/$url_path
    resp=$(perform-get $url)

    [[ $? -ne 0 ]] && exit $?

    provider=$(echo $resp | jq .openid_provider_list) 2>/dev/null

    if [[ $? -ne 0 ]]
    then
	>&2 echo Something other than JSON returned from server
    fi


    ids=($(echo $provider | jq -c '.[].id'))
    issuer=($(echo $provider | jq -c '.[].issuer'))

    #echo "id  issuer"
    for ((i=0; i<${#ids[@]}; i++))
    do
	echo ${ids[$i]} ${issuer[$i]}
    done
}

function request-endservice-access {
    host=$1
    sid=$2
    at=$3
    iss=$4
    url_path=api/credential
    url=$URI_SCHEME://$host/$url_path

    body="{\"service_id\":\"$sid\"}"
    perform-post $url $at $body $iss
}

function revoke-endservice-access {
    host=$1
    cstate=$2
    at=$3
    iss=$4

    url_path=api/credential/$cstate
    url=$URI_SCHEME://$host/$url_path
    perform-delete $url $at $iss
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
    lscred)
	list-credentials $@
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
	echo '  # List all OpenId Connect providers'
	echo '    lsprov  <host>'
	echo '  # List all end-services provided to the user'
	echo '    lsserv  <host> <ac_token> <issuer>'
	echo '  # List all credential references of the user'
	echo '    lscred  <host> <ac_token> <issuer>'
	echo '  # Request credentials for a service'
	echo '    request <host> <service_id> <ac_token> <issuer>'
    echo '  # Revoke the credentials, given by the credential id (cred_id)'
	echo '    revoke  <host> <cred_id> <ac_token> <issuer>'
	;;
    *)
	echo 'Unknown command'
	echo "Try $0 --help|-h|help"
	exit 1
	;;
esac

