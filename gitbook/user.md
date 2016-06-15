### Login and Logout
#### Login
Open the Token Translation Service web-interface in your browser. You will see a
a page which contains a drop down field and a `Login` button.

Select your OpenId Connect Provider in the drop down field and press the login
button. This will redirect your browser to the Provider you selected.

Login into your account at the provider page. If this is your first time
connecting to this TTS instance you will be asked for your permissions to
forward informations to the TTS.
After logging in at your provider you will be sent back to the TTS and
automatically recognized as the user.

If you see the login page again, your login failed or you are not allowed to use
this TTS.

#### Logout
After Using the TTS you should logout, so no one else can create credentials at
your computer. 
To logout simply press the `Logout` button at the lower border of the page.
After a succesfull logout you will see the login page again.

### Creating And Revoking Credentials
Once logged in you see one or two tables:
- Services, this one should always be visible.
- Credentials, this might be hidden, if you have no credentials.

#### Creating Credentials 
In the Services table each line is a service for which you can request a
credential.
You request a credential by simply pressing the `Request` button.

If the request was succesful a dialog will pop up with all relevant informations
of the credential and how to use it. 
Once you close this dialog all informations are gone! The TTS does *not* store
the credentials. It only stores a reference to the credentials you created so
they can be removed, if needed.

### Revoking Credentials
If you created credentials before, you see the Credentials Table.
Each line representing a credential.
By simlpy clicking the `Revoke` button the TTS revokes your credential and it
will be removed from the list of credentials.

### Additional Information
#### Show Access Token
After pressing the `Show Access Token` button a dialog opens up. This dialog
contains the so called OpenId Connect AccessToken, which can be used to act upon
your behalf. It is e.g. used to request credentials using the REST Api.

In general you shouldn't need to use it, unless you are a developer.

