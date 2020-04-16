# TO-DO list 

- ~~provide a way to specify configuration for the haskell server that will output
  the json user wants (route, port etc.)~~
- the configuration shall be done through a minimal web interface (yesod with sqlite)
- I am thinking in terms of routes, so user should define a ~~config file~~ configuration that
  shows how to access some api. 
- User should be able to specify a route which should return certain
  json, html, xml or whatever (user provided).
  Then we should have a recipe on how each api should retrieve correct
  information (or if it should ommit it).
  So like mapping of expected output to existing input comming in from the api response.
- User will then have access to a server that precisely outputs the json user
  expects and the data is comming in from different api sources. (That implies
  json structure should be a list of some values or specific objects but that
  can be configured by the user)

# Step 1:
- install yesod and add option to add api services user is interested in.
- The fields that user needs to input are:
 - api name
 - api url
 - api login (user/pass, how to get a token, cert file etc.)
 We should have a button that provides immediate call to check if we're able to
 talk to the api in question.
