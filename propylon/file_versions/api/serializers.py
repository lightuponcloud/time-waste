from rest_framework import serializers

from propylon_document_manager.file_versions.models import (
    FileVersion, FileModel)


class FileVersionSerializer(serializers.ModelSerializer):
    class Meta:
        model = FileVersion
        fields = "__all__"


class FileModelSerializer(serializers.ModelSerializer):
    class Meta:
        model = FileModel
        fields = "__all__"
