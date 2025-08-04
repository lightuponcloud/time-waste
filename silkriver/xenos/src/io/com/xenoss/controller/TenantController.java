package io.com.xenoss.controller;

import io.com.xenoss.model.Tenant;
import io.com.xenoss.service.TenantService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api/tenants")
@CrossOrigin(origins = "*")
public class TenantController {

    @Autowired
    private TenantService TenantService;

    @PostMapping
    public ResponseEntity<Tenant> createTenant(@Valid @RequestBody Tenant Tenant) {
        try {
            Tenant createdTenant = TenantService.createTenant(Tenant);
            return new ResponseEntity<>(createdTenant, HttpStatus.CREATED);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(null, HttpStatus.CONFLICT);
        }
    }

    @GetMapping
    public ResponseEntity<List<Tenant>> getAllTenants() {
        List<Tenant> Tenants = TenantService.getAllTenants();
        return new ResponseEntity<>(Tenants, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<Tenant> getTenantById(@PathVariable Long id) {
        Optional<Tenant> Tenant = TenantService.getTenantById(id);
        return Tenant.map(u -> new ResponseEntity<>(u, HttpStatus.OK))
                .orElse(new ResponseEntity<>(HttpStatus.NOT_FOUND));
    }

    @GetMapping("/search/{orgCode}")
    public ResponseEntity<List<Tenant>> searchTenantsByLastName(@PathVariable String name) {
        List<Tenant> Tenants = TenantService.searchTenantsByName(name);
        return new ResponseEntity<>(Tenants, HttpStatus.OK);
    }

    @PutMapping("/{id}")
    public ResponseEntity<Tenant> updateTenant(@PathVariable Long id, @Valid @RequestBody Tenant Tenant) {
        try {
            Tenant updatedTenant = TenantService.updateTenant(id, Tenant);
            return new ResponseEntity<>(updatedTenant, HttpStatus.OK);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(null, HttpStatus.NOT_FOUND);
        }
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteTenant(@PathVariable Long id) {
        boolean deleted = TenantService.deleteTenant(id);
        return deleted ? new ResponseEntity<>(HttpStatus.NO_CONTENT)
                : new ResponseEntity<>(HttpStatus.NOT_FOUND);
    }

    @GetMapping("/count")
    public ResponseEntity<Long> getTenantCount() {
        long count = TenantService.getTenantCount();
        return new ResponseEntity<>(count, HttpStatus.OK);
    }

    @GetMapping("/{id}/exists")
    public ResponseEntity<Boolean> TenantExists(@PathVariable Long id) {
        boolean exists = TenantService.TenantExists(id);
        return new ResponseEntity<>(exists, HttpStatus.OK);
    }
}