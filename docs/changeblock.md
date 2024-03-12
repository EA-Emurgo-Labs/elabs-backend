# ChangeBlock related notes

The smart contract of ChangeBlock caused issues with the backend wallet due to
its unconventional design. As a result, we have limited the use of the backend
wallet.

* We modified the wallet backend to ensure that every user has exactly one
  unique address assigned to them.
