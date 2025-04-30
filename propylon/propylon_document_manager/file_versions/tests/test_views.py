import pytest
from unittest.mock import MagicMock, patch

from django.conf import settings
from django.contrib import messages
from django.contrib.auth.models import AnonymousUser
from django.contrib.messages.middleware import MessageMiddleware
from django.contrib.sessions.middleware import SessionMiddleware
from django.http import HttpRequest, HttpResponseRedirect
from django.test import RequestFactory
from django.urls import reverse
from django.utils.translation import gettext_lazy as _
from django.core.files.uploadedfile import SimpleUploadedFile

from propylon_document_manager.file_versions.tests.factories import FileModelFactory
from propylon_document_manager.users.models import User
from propylon_document_manager.file_versions.models import (
    FileModel,
    FileVersion)
from propylon_document_manager.file_versions.api.views import (
    FileVersionViewSet)


pytestmark = pytest.mark.django_db


class TestFileVersionsView:

    def dummy_get_response(self, request: HttpRequest):
        return None

    def test_get_success_url(self, user: User, rf: RequestFactory):
        view = FileVersionViewSet()
        request = rf.get("/fake-url/")
        request.user = user

        view.request = request
        assert view.get_success_url() == "/file_versions/"

    def test_get_object(self, user: User, rf: RequestFactory):
        view = FileVersionViewSet()
        request = rf.get("/fake-url/")
        request.user = user

        view.request = request

        assert view.get_queryset().count() == 0

    def test_upload_download(self, user: User, rf: RequestFactory):
        view = FileVersionViewSet()
        request = rf.get("/fake-url/")
        request.user = user

        view.request = request

        fake_file = MagicMock()
        fake_file.name = "something.txt"
        fake_file.__len__ = MagicMock(return_value=5)
        fake_file.read = MagicMock(return_value=b"blah1")

        fake_request = MagicMock()
        fake_request.FILES = {"file": fake_file}
        fake_request.POST = {"version_number": 1}
        view._check_token = MagicMock(return_value=user)

        response = view.post(fake_request)
        version_id = response.data
        assert version_id

        version_obj = FileVersion.objects.get(id=version_id)
        assert version_obj.version_number == 1
        assert version_obj.file
        assert version_obj.file.size == 5
        assert version_obj.file.fn == "something.txt"
        assert version_obj.file.added_email == user.email

        # test download
        fake_request = MagicMock()
        response = view.download(fake_request, id=version_obj.file.id)
        assert response.data == b"blah1"
