from django.urls import resolve, reverse

from propylon_document_manager.file_versions.models import FileVersion


def test_detail(file_version: FileVersion):
    assert reverse("file_versions:detail", kwargs={"pk": file_version.pk}) == f"/file_versions/{file_version.pk}/"
