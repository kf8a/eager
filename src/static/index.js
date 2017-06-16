var Elm = require( '../elm/Main' );

var options = {
  allowedConnections: ['Username-Password-Authentication']
};
var lock = new Auth0Lock('tbhDQ687pndxPaPmJZ27MHjXCgeJWlxB', 'lter.auth0.com', options);
var storedProfile = localStorage.getItem('profile');
var storedToken = localStorage.getItem('token');
var authData = storedProfile && storedToken ? { profile: JSON.parse(storedProfile), token: storedToken } : null;
var elmNode = document.getElementById('main');
var elmApp = Elm.Main.embed(elmNode,authData);

// Show Auth0 lock subscription
elmApp.ports.auth0showLock.subscribe(function(opts) {
  lock.show();
});

// Log out of Auth0 subscription
elmApp.ports.auth0logout.subscribe(function(opts) {
  localStorage.removeItem('profile');
  localStorage.removeItem('token');
});

// Listening for the authenticated event
lock.on("authenticated", function(authResult) {
  // Use the token in authResult to getProfile() and save it to localStorage
  lock.getProfile(authResult.idToken, function(err, profile) {
    var result = { err: null, ok: null };
    var token = authResult.idToken;

    if (!err) {
      result.ok = { profile: profile, token: token };
      localStorage.setItem('profile', JSON.stringify(profile));
      localStorage.setItem('token', token);
    } else {
      result.err = err.details;

      // Ensure that optional fields are on the object
      result.err.name = result.err.name ? result.err.name : null;
      result.err.code = result.err.code ? result.err.code : null;
      result.err.statusCode = result.err.statusCode ? result.err.statusCode : null;
    }
    elmApp.ports.auth0authResult.send(result);
  });
});

