# User Guide
Using the web interface of WaTTS is quite simple. Basically, the only thing
one needs to do is to press certain buttons and follow the instructions.

## Login and Logout
### Login
Open the Token Translation Service web-interface in the browser. You will see a
a page which contains a drop down field and a `Login` button.

Select your OpenId Connect Provider in the drop down field and press the login
button. This will redirect the browser to the selected Provider.

Login into your account at the provider page. If this is the first time
connecting to this WaTTS instance, you will be asked for the permissions to
forward information to WaTTS.
After logging in at your provider, you will be sent back to WaTTS and
automatically recognized as a user.

If you see the login page again, your login has failed or you are not allowed to use
this WaTTS.

### Logout
After using WaTTS you should logout, so that no one else can create credentials using
your computer.
To logout, simply press the `Logout` button at the lower border of the page.
After a successful logout you will see the login page again.

## Creating And Revoking Credentials
Once logged in, you will see one or two tables:
- Services table, which is always visible.
- Credentials table, which might be hidden, if you have no credentials.

### Creating Credentials
In the Services table each line is a service for which one can request a
credential.
You request a credential by simply pressing the `Request` button. The service might
be disable for you, then the serivce is grey and the `Request` button disabled.

If the `Advanced` button is enabled it means that the service accepts parameter.
Pressing the button opens a dialog where informations can be entered. If there are
more than one configuration possibility arrows are shown to change the configurations.
An example for the advanced button is deployment of ssh keys, where you can paste your
public key when using the advanced button.

If the request was successful, a dialog will pop up with all the relevant information
of the credential and how to use it.
Once you close this dialog, all information is gone! WaTTS does *not* store
the credentials. It only stores a reference to the credentials you created so
that they can be removed, if needed.

### Revoking Credentials
If you created credentials before, you will see the Credentials Table.
Each line represents a credential.
By simply clicking the `Revoke` button WaTTS revokes your credential and it
will be removed from the list of credentials.

## Additional Information
### Show Access Token
After pressing the `Show Access Token` button a dialog appears. This dialog
contains the so-called OpenId Connect AccessToken, which can be used to act upon
your behalf. It is e.g. used to request credentials using the REST API.

In general, there is no need to use it, unless working in development.
