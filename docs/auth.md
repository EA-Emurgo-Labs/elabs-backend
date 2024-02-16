# Authorization header

All requestion require an authorization header.
```bash
curl -H "Authorization: <TOKEN>" -XGET http://localhost:8081/api/v0/wallet/1
```

Tokens are saved on the `Auth` DB table, and all tokens are loaded during the startup into the env to prevent querying the DB for every request. It is
best to send a few tokens in advance.
