package io.com.xenoss.controller;

import io.com.xenoss.model.Asset;
import io.com.xenoss.service.AssetService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api/assets")
@CrossOrigin(origins = "*")
public class AssetController {

    @Autowired
    private AssetService AssetService;

    @PostMapping
    public ResponseEntity<Asset> createAsset(@Valid @RequestBody Asset Asset) {
        try {
            Asset createdAsset = AssetService.createAsset(Asset);
            return new ResponseEntity<>(createdAsset, HttpStatus.CREATED);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(null, HttpStatus.BAD_REQUEST);
        }
    }

    @GetMapping
    public ResponseEntity<List<Asset>> getAllAssets() {
        List<Asset> Assets = AssetService.getAllAssets();
        return new ResponseEntity<>(Assets, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<Asset> getAssetById(@PathVariable Long id) {
        Optional<Asset> Asset = AssetService.getAssetById(id);
        return Asset.map(o -> new ResponseEntity<>(o, HttpStatus.OK))
                .orElse(new ResponseEntity<>(HttpStatus.NOT_FOUND));
    }

    @GetMapping("/number/{AssetNumber}")
    public ResponseEntity<Asset> getAssetByNumber(@PathVariable String AssetNumber) {
        Optional<Asset> Asset = AssetService.getAssetByNumber(AssetNumber);
        return Asset.map(o -> new ResponseEntity<>(o, HttpStatus.OK))
                .orElse(new ResponseEntity<>(HttpStatus.NOT_FOUND));
    }

    @PutMapping("/{id}")
    public ResponseEntity<Asset> updateAsset(@PathVariable Long id, @Valid @RequestBody Asset Asset) {
        try {
            Asset updatedAsset = AssetService.updateAsset(id, Asset);
            return new ResponseEntity<>(updatedAsset, HttpStatus.OK);
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(null, HttpStatus.NOT_FOUND);
        }
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteAsset(@PathVariable Long id) {
        boolean deleted = AssetService.deleteAsset(id);
        return deleted ? new ResponseEntity<>(HttpStatus.NO_CONTENT)
                : new ResponseEntity<>(HttpStatus.NOT_FOUND);
    }

    @GetMapping("/count")
    public ResponseEntity<Long> getAssetCount() {
        long count = AssetService.getAssetCount();
        return new ResponseEntity<>(count, HttpStatus.OK);
    }

}