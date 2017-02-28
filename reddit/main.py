import requests
import requests.auth

client_auth = requests.auth.HTTPBasicAuth('m_tkLKYv01pFg', 'CIIyi7Sum-YHy1E9LNhFRIyTn6Y')
post_data = {"grant_type": "password", "username": "TheSonOfMyWife", "password": "bzZs3th3"}
headers = {"User-Agent": "ChangeMeClient/0.1 by YourUsername"}
response = requests.post("https://www.reddit.com/api/v1/access_token", auth=client_auth, data=post_data, headers=headers)
response.json()
