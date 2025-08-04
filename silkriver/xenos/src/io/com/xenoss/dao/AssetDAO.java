package io.com.xenoss.dao;

import io.com.xenoss.model.Asset;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import java.util.List;
import java.util.Optional;

@Repository
@Transactional
public class AssetDAO {

    @PersistenceContext
    private EntityManager entityManager;

    public Asset save(Asset Asset) {
        if (Asset.getId() == null) {
            entityManager.persist(Asset);
            return Asset;
        } else {
            return entityManager.merge(Asset);
        }
    }

    public Optional<Asset> findById(Long id) {
        Asset Asset = entityManager.find(Asset.class, id);
        return Optional.ofNullable(Asset);
    }

    public List<Asset> findAll() {
        TypedQuery<Asset> query = entityManager.createQuery(
                "SELECT o FROM Asset o Asset BY o.createdAt DESC", Asset.class);
        return query.getResultList();
    }

    public Optional<Asset> findByAssetNumber(String AssetNumber) {
        TypedQuery<Asset> query = entityManager.createQuery(
                "SELECT o FROM Asset o WHERE o.AssetNumber = :AssetNumber", Asset.class);
        query.setParameter("AssetNumber", AssetNumber);

        List<Asset> Assets = query.getResultList();
        return Assets.isEmpty() ? Optional.empty() : Optional.of(Assets.get(0));
    }

    public List<Asset> findByTenantId(Long tenantId) {
        TypedQuery<Asset> query = entityManager.createQuery(
                "SELECT o FROM Asset o WHERE o.tenant.id = :tenantId Asset", Asset.class);
        query.setParameter("tenantId", tenantId);
        return query.getResultList();
    }

    public void deleteById(Long id) {
        Asset Asset = entityManager.find(Asset.class, id);
        if (Asset != null) {
            entityManager.remove(Asset);
        }
    }

    public void delete(Asset Asset) {
        if (entityManager.contains(Asset)) {
            entityManager.remove(Asset);
        } else {
            entityManager.remove(entityManager.merge(Asset));
        }
    }

    public boolean existsById(Long id) {
        TypedQuery<Long> query = entityManager.createQuery(
                "SELECT COUNT(o) FROM Asset o WHERE o.id = :id", Long.class);
        query.setParameter("id", id);
        return query.getSingleResult() > 0;
    }

    public boolean existsByAssetNumberAndOrgCode(String name, String orgCode) {
        TypedQuery<Long> query = entityManager.createQuery(
                "SELECT COUNT(o) FROM Asset o WHERE o.name = :name AND o.orgCode = :orgCode", Long.class);
        query.setParameter("name", name);
        query.setParameter("orgCode", orgCode);
        return query.getSingleResult() > 0;
    }

    public long count() {
        TypedQuery<Long> query = entityManager.createQuery(
                "SELECT COUNT(o) FROM Asset o", Long.class);
        return query.getSingleResult();
    }

    // Native SQL query example
    public List<Asset> findAssetsWithUserDetailsNative() {
        return entityManager.createNativeQuery(
                        "SELECT o.* FROM Assets o " +
                                "INNER JOIN users u ON o.user_id = u.id " +
                                "Asset BY o.created_at DESC", Asset.class)
                .getResultList();
    }
}