from django.shortcuts import render
from django.views.generic import DetailView, RedirectView, UpdateView, CreateView
from django.contrib.auth.mixins import LoginRequiredMixin
from django.contrib.auth.models import AnonymousUser
from django.contrib.messages.views import SuccessMessageMixin
from django.utils.translation import gettext_lazy as _
from django.urls import reverse
from django.http import Http404
from django.http import HttpResponseForbidden

from rest_framework import status
from rest_framework.response import Response
from rest_framework.authtoken.models import Token
from rest_framework.mixins import RetrieveModelMixin, ListModelMixin
from rest_framework.viewsets import GenericViewSet
from rest_framework.decorators import action

from propylon_document_manager.file_versions.models import FileVersion, FileModel
from .serializers import FileVersionSerializer, FileModelSerializer


class FileVersionViewSet(RetrieveModelMixin, ListModelMixin, GenericViewSet):
    authentication_classes = []
    permission_classes = []
    serializer_class = FileVersionSerializer
    queryset = FileVersion.objects.all()
    lookup_field = "id"

    def get_queryset(self):
        user = self.request.user
        if isinstance(user, AnonymousUser):
            return FileVersion.objects.none()
        return FileVersion.objects.filter(file__added_email=user.email)

    @action(methods=['get'], detail=True)
    def download(self, request, id=None):
        """
        Allows users to fetch any revision of any file

        Does not allow a user to access files submitted by another user
        """
        user = self._check_token(request)
        if not user:
            return HttpResponseForbidden("Access denied.")

        try:
            version = FileVersion.objects.get(id=id)
        except FileVersion.DoesNotExist:
            raise Http404

        fobj = version.file
        if not fobj.can_read():
            return HttpResponseForbidden("Access denied.")

        if not fobj.can_read():
            return HttpResponseForbidden("Can't read file.")

        with open(fobj.full_path, "rb") as fd:
            response = Response(fd.read(),
                                    content_type='application/force-download')
            response['Content-Disposition'] = "attachment;filename=\"%s\"" % fobj.basename
            response['Content-length'] = fobj.size
            return response

        return Response("")

    def post(self, request):
        """
        Stores files of any type and name

        Does not allow interaction by non-authenticated users

        Allows users to store multiple revisions of the same file at the same URL

        """
        user = self._check_token(request)
        if not user:
            return HttpResponseForbidden("Access denied.")

        if request.FILES:
            file_obj = request.FILES['file']

            obj = FileModel()
            obj.fn = file_obj.name
            obj.size = len(file_obj)
            obj.added_email = user.email
            obj.save()

            dst = obj.full_path
            with open(dst, "wb+") as fd:
                fd.write(file_obj.read())

            try:
                version_number = int(request.POST.get("version_number"))
            except (TypeError, ValueError):
                version_number = 1

            version = FileVersion()
            version.file = obj
            version.version_number = version_number
            version.save()

            return Response(version.id, status.HTTP_201_CREATED)

    def _check_token(self, request):
        auth_header = request.headers['Authorization']
        if not auth_header:
            return None
        token = auth_header.split('Token ')[1]

        try:
            token = Token.objects.get(key=token)
        except Token.DoesNotExist:
            return None
        return token.user

    def get_success_url(self):
        assert self.request.user.is_authenticated  # for mypy to know that the user is authenticated
        return reverse("file_versions:api")
