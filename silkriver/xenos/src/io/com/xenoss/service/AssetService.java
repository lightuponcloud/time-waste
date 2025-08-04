package io.com.xenoss.service;

import io.com.xenoss.dao.AssetDAO;
import io.com.xenoss.dao.TenantDAO;
import io.com.xenoss.model.Asset;
import io.com.xenoss.model.Tenant;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
@Transactional
public class AssetService {

    @Autowired
    private AssetDAO AssetDAO;

    @Autowired
    private TenantDAO TenantDAO;

    public Asset createAsset(Asset asset) {
        if (AssetDAO.existsByAssetNumberAndOrgCode(asset.getName(), asset.getOrgCode())) {
            throw new IllegalArgumentException("Asset with name " + asset.getName() + " and orgCode " + asset.getOrgCode() + "already exists");
        }

        if (asset.getOrgCode() == null || asset.getName() == null) {
            throw new IllegalArgumentException("Asset must have a valid name and orgCode");
        }

        Optional<Tenant> Tenant = TenantDAO.findById(asset.getTenant().getId());
        if (Tenant.isEmpty()) {
            throw new IllegalArgumentException("Tenant with id " + asset.getTenant().getId() + " not found");
        }

        asset.setTenant(Tenant.get());
        return AssetDAO.save(asset);
    }

    public Optional<Asset> getAssetById(Long id) {
        return AssetDAO.findById(id);
    }

    public List<Asset> getAllAssets() {
        return AssetDAO.findAll();
    }

    public Optional<Asset> getAssetByNumber(String AssetNumber) {
        return AssetDAO.findByAssetNumber(AssetNumber);
    }

    public List<Asset> getAssetsByTenantId(Long tenantId) {
        return AssetDAO.findByTenantId(tenantId);
    }

    public Asset updateAsset(Long id, Asset updatedAsset) {
        Optional<Asset> existingAsset = AssetDAO.findById(id);
        if (existingAsset.isEmpty()) {
            throw new IllegalArgumentException("asset with id " + id + " not found");
        }

        Asset asset = existingAsset.get();

        // Check if asset number is being changed and if it conflicts
        if (!asset.getName().equals(updatedAsset.getName()) &&
                AssetDAO.existsByAssetNumberAndOrgCode(updatedAsset.getName(), updatedAsset.getOrgCode())) {
            throw new IllegalArgumentException("Asset with name " + updatedAsset.getName() + " and orgCode " + asset.getOrgCode() + "is already in use");
        }

        asset.setName(updatedAsset.getName());
        asset.setOrgCode(updatedAsset.getOrgCode());
        return AssetDAO.save(asset);
    }

    public boolean deleteAsset(Long id) {
        if (AssetDAO.existsById(id)) {
            AssetDAO.deleteById(id);
            return true;
        }
        return false;
    }

    public long getAssetCount() {
        return AssetDAO.count();
    }

    public boolean AssetExists(Long id) {
        return AssetDAO.existsById(id);
    }
}