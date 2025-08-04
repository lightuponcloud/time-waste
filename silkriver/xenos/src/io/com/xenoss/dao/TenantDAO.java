package io.com.xenoss.dao;

import io.com.xenoss.model.Tenant;
import io.com.xenoss.model.TenantStatus;
import org.hibernate.Session;
import org.hibernate.query.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import java.util.List;
import java.util.Optional;

@Repository
@Transactional
public class TenantDAO {

    @PersistenceContext
    private EntityManager entityManager;

    public Tenant save(Tenant Tenant) {
        if (Tenant.getId() == null) {
            entityManager.persist(Tenant);
            return Tenant;
        } else {
            return entityManager.merge(Tenant);
        }
    }

    public Optional<Tenant> findById(Long id) {
        Tenant Tenant = entityManager.find(Tenant.class, id);
        return Optional.ofNullable(Tenant);
    }

    public List<Tenant> findAll() {
        TypedQuery<Tenant> query = entityManager.createQuery(
                "SELECT u FROM Tenant u ORDER BY u.name, u.orgCode", Tenant.class);
        return query.getResultList();
    }

    public List<Tenant> findByNameContaining(String name) {
        TypedQuery<Tenant> query = entityManager.createQuery(
                "SELECT u FROM Tenant u WHERE LOWER(u.name) LIKE LOWER(:name)", Tenant.class);
        query.setParameter("name", "%" + name + "%");
        return query.getResultList();
    }

    public List<Tenant> findByLastNameContaining(String lastName) {
        TypedQuery<Tenant> query = entityManager.createQuery(
                "SELECT u FROM Tenant u WHERE LOWER(u.lastName) LIKE LOWER(:lastName)", Tenant.class);
        query.setParameter("lastName", "%" + lastName + "%");
        return query.getResultList();
    }

    public void deleteById(Long id) {
        Tenant Tenant = entityManager.find(Tenant.class, id);
        if (Tenant != null) {
            entityManager.remove(Tenant);
        }
    }

    public void delete(Tenant Tenant) {
        if (entityManager.contains(Tenant)) {
            entityManager.remove(Tenant);
        } else {
            entityManager.remove(entityManager.merge(Tenant));
        }
    }

    public boolean existsById(Long id) {
        TypedQuery<Long> query = entityManager.createQuery(
                "SELECT COUNT(u) FROM Tenant u WHERE u.id = :id", Long.class);
        query.setParameter("id", id);
        return query.getSingleResult() > 0;
    }

    public boolean existsByName(String name) {
        TypedQuery<Long> query = entityManager.createQuery(
                "SELECT COUNT(u) FROM Tenant u WHERE u.name = :name", Long.class);
        query.setParameter("name", name);
        return query.getSingleResult() > 0;
    }

    public long count() {
        TypedQuery<Long> query = entityManager.createQuery(
                "SELECT COUNT(u) FROM Tenant u", Long.class);
        return query.getSingleResult();
    }

    // Native SQL query example
    public List<Tenant> findTenantsWithOrdersNative() {
        return entityManager.createNativeQuery(
                        "SELECT DISTINCT u.* FROM Tenants u " +
                                "INNER JOIN orders o ON u.id = o.Tenant_id", Tenant.class)
                .getResultList();
    }

    // Hibernate Session API example (alternative approach)
    public List<Tenant> findTenantsUsingHibernateSession() {
        Session session = entityManager.unwrap(Session.class);
        Query<Tenant> query = session.createQuery(
                "FROM Tenant u ORDER BY u.lastName", Tenant.class);
        return query.getResultList();
    }

    public long countByStatus(TenantStatus status) {
        TypedQuery<Long> query = entityManager.createQuery(
                "SELECT COUNT(o) FROM Asset o WHERE o.status = :status", Long.class);
        query.setParameter("status", status);
        return query.getSingleResult();
    }

}