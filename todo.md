# TO-DO list 

- provide a way to specify configuration for the haskell server that will output
  the json user wants (route, port etc.)
- provide a way to specify configuration for each api endpoint we
  want to call 
- I am thinking in terms of routes, so user should define a config file that
  shows how to access some api and then a route which should return certain
  json, html, xml or whatever (user provided). Then we should have a recipe on
  how each api should retrieve correct information. So like mapping of expected
  output to existing input comming in from the api response.
- User will then have access to a server that precisely outputs the json user
  expects and the data is comming in from different api sources. (That implies
  json structure should be a list of some values or specific objects but that
  can be configured by the user)

