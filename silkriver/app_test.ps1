Invoke-RestMethod -Uri "http://localhost:8080/api/tenants" `
    -Method Post `
    -ContentType "application/json" `
    -Body (@{
        name = "Tenant1"
        orgCode     = "East"
    } | ConvertTo-Json)

exit 0
