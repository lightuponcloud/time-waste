from django.urls import path

from propylon_document_manager.file_versions.api.views import (
    FileVersionViewSet)

app_name = "file_versions"
urlpatterns = [
    path("", view=FileVersionViewSet.as_view({'get': 'list'}), name="api"),
]
