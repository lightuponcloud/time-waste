import os
from datetime import datetime

from django.db import models
from django.conf import settings
from django.db.models.deletion import CASCADE, Collector

from propylon_document_manager.users.models import BaseRestrictedModel


class FileModel(BaseRestrictedModel):
    """
    Store files on filesystem by the path, based on FK.

    Converts FK id to 9-digit filename, then split this string to 3 parts and store
    file like 000/000/001/000000001
    """
    # original filename
    fn = models.TextField()
    # store sha1 of original images
    sha1 = models.CharField(max_length=40, blank=True, null=True)
    size = models.IntegerField()
    date_added = models.DateField(auto_now_add=True)

    class Meta:
        verbose_name = "File"
        verbose_name_plural = "Files"
        app_label = "file_versions"

    def _get_serial(self):
        return "%06d"%self.id
    serial = property(_get_serial)

    def _relative_path(self):
        path = self.full_path
        if not path:
            return ""
        root=settings.STORE_ROOT
        return path.split(root)[1][1:]
    relative_path = property(_relative_path)

    def _full_path(self):
        if not self.pk:
            return ""
        root = settings.STORE_ROOT
        if not os.path.exists(root):
            return ""
        if not root.endswith("/"):
            root = u"%s/"%root
        fn = u"%0.9d" % self.id
        for t in [fn[0:3], fn[3:6]]:
            d = os.path.join(root, t)
            if not os.path.exists(d):
                os.mkdir(d)
            root += t+u"/"
        return os.path.abspath(os.path.join(root, fn))
    full_path = property(_full_path)

    def _basename(self):
        return u"%0.9d"%self.id
    basename = property(_basename)

    def can_read(self):
        path = self.full_path
        if os.path.exists(path):
            return os.access(path, os.R_OK)
        return False

    def can_write(self):
        path = self.full_path
        if os.path.exists(path):
            return os.access(path, os.W_OK)
        return False

    def _get_updated_date(self):
        path = self.full_path
        if os.path.exists(path):
            return datetime.fromtimestamp(os.path.getmtime(path))
        return None
    date_updated = property(_get_updated_date)

    def _get_extenstion(self):
        return os.path.splitext(self.fn)[1]
    extension = property(_get_extenstion)

    def get_filesize(self):
        try:
            return os.path.getsize(self.full_path)
        except:
            return 0

    def delete(self):
        try:
            os.unlink(self.full_path)
        except:
            pass
        super(FileModel, self).delete()


class FileVersion(models.Model):
    """
    Stores File Versions
    """
    file = models.ForeignKey(FileModel, related_name="versions",
                             on_delete=CASCADE, default=None)
    version_number = models.fields.IntegerField()

    class Meta:
        verbose_name = "File Version"
        verbose_name_plural = "File Versions"
        app_label = "file_versions"
