package io.com.xenoss.service;

import io.com.xenoss.dao.TenantDAO;
import io.com.xenoss.model.Tenant;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class TenantService {

    @Autowired
    private TenantDAO TenantDAO;

    public Tenant createTenant(Tenant Tenant) {
        if (TenantDAO.existsByName(Tenant.getName())) {
            throw new IllegalArgumentException("Tenant with name " + Tenant.getName() + " already exists");
        }
        return TenantDAO.save(Tenant);
    }

    public Optional<Tenant> getTenantById(Long id) {
        return TenantDAO.findById(id);
    }

    public List<Tenant> getAllTenants() {
        return TenantDAO.findAll();
    }

    public List<Tenant> searchTenantsByName(String name) {
        return TenantDAO.findByLastNameContaining(name);
    }

    public Tenant updateTenant(Long id, Tenant updatedTenant) {
        Optional<Tenant> existingTenant = TenantDAO.findById(id);
        if (existingTenant.isEmpty()) {
            throw new IllegalArgumentException("Tenant with id " + id + " not found");
        }

        Tenant Tenant = existingTenant.get();
        Tenant.setName(updatedTenant.getName());
        Tenant.setStatus(updatedTenant.getStatus());

        return TenantDAO.save(Tenant);
    }

    public boolean deleteTenant(Long id) {
        if (TenantDAO.existsById(id)) {
            TenantDAO.deleteById(id);
            return true;
        }
        return false;
    }

    public long getTenantCount() {
        return TenantDAO.count();
    }

    public boolean TenantExists(Long id) {
        return TenantDAO.existsById(id);
    }

}