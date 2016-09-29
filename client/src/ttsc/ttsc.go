package main

import (
       "os"
       "fmt"
       "net/http"
       "crypto/tls"
       "github.com/dghubble/sling"
       )

const api_base="/api/v2/"

type OpenIdProvider struct {
    Id    string `json:"id"`
    Desc  string `json:"desc"`
    Issuer string `json:"issuer"`
    Ready bool `json:"ready"`
}

func (provider OpenIdProvider) String() (string) {
     prefix := " - {DISABLED}"
     if provider.Ready {
        prefix = " -"
     }
     return fmt.Sprintf("%s %s: %s [%s]", prefix, provider.Id, provider.Desc, provider.Issuer)
}

func (plist OpenIdProviderList) String() (string) {
     text := fmt.Sprintln("List of OpenId Connect Provider")
     descProvider := OpenIdProvider{"<id>", "<description>", "<issuer url>", true}
    text += fmt.Sprintln(descProvider)
    for _, v := range plist.Provider {
        text += fmt.Sprintln(v)
    }
    return text
}

type OpenIdProviderList struct {
    Provider  []OpenIdProvider `json:"openid_provider_list"`
}

type TTSError struct {
    Error  string `json:"error"`
}

func show_help() {
fmt.Println("Usage:")
fmt.Println("  # List all OpenId Connect providers")
fmt.Println("    lsprov  <host>")
fmt.Println("  # List all end-services provided to the user")
fmt.Println("    lsserv  <host> <ac_token> <issuer>")
fmt.Println("  # List all credential references of the user")
fmt.Println("    lscred  <host> <ac_token> <issuer>")
fmt.Println("  # Request credentials for a service")
fmt.Println("    request <host> <service_id> <ac_token> <issuer>")
fmt.Println("  # Revoke the credentials, given by the credential id (cred_id)")
fmt.Println("    revoke  <host> <cred_id> <ac_token> <issuer>")
}

func get_scheme() (string) {
    _, set := os.LookupEnv("TTSC_NOSSL")
    if set {
       return "http://"
    } else {
       return "https://"
    }
}

func client() (*http.Client) {
    _, set := os.LookupEnv("TTSC_INSECURE")
    if set {
       tr := &http.Transport{
          TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
       }
       return &http.Client{Transport: tr}
    } else {
        return http.DefaultClient
    }
}

func list_provider(host string) {
     providerList := new(OpenIdProviderList)
     ttsError := new(TTSError)
     client := client()
     _, err := sling.New().Client(client).Get(host).Path(api_base).Path("oidcp").Receive(providerList, ttsError)
     if err != nil {
        fmt.Println("error requesting list of providers: ",err)
        return
     }
     fmt.Println(providerList)
}

func list_services(host, token, issuer string) {
     fmt.Println("listing services")
}

func list_credentials(host, token, issuer string) {
     fmt.Println("listing credentials")
}

func request(host, serviceId, token, issuer string) {
     fmt.Println("requesting credential")
}

func revoke(host, credId, token, issuer string) {
     fmt.Println("revoking credential")
}

func main() {
    args := os.Args[1:]
    argsNum := len(args)
    if argsNum < 2 {
       show_help()
       return
    }
    cmd := args[0]
    host := get_scheme()
    host += args[1]

    switch cmd {
    case "lsprov":
         list_provider(host)
    case "lsserv":
         if argsNum == 4 {
            list_services(host, args[2], args[3])
         } else {
           show_help()
         }
    case "lscred":
         if argsNum == 4 {
            list_credentials(host, args[2], args[3])
         } else {
           show_help()
         }
    case "request":
         if argsNum == 5 {
            request(host, args[2], args[3], args[4])
         } else {
           show_help()
         }
    case "revoke":
         if argsNum == 5 {
            revoke(host, args[2], args[3], args[4])
         } else {
           show_help()
         }
    default:
        show_help()
    }
}