import hashlib

from django.core.management.base import BaseCommand, CommandError

from propylon_document_manager.file_versions.models import (
    FileVersion, FileModel)
from propylon_document_manager.users.models import User

file_versions = [
    'bill_document',
    'amendment_document',
    'act_document',
    'statute_document',
]


def create_dummy_user(email):
    # Create a dummy user
    user = User()
    user.email = email
    user.name = "Someone"
    user.first_name = "Some"
    user.last_name = "One"
    user.username = "someone"
    # user.set_unusable_password()
    user.set_password("blah")
    user.save()


class Command(BaseCommand):
    help = "Load basic file version fixtures"

    def handle(self, *args, **options):
        if User.objects.count() == 0:
            create_dummy_user("example@example.com")
            create_dummy_user("example2@example.com")
        for file_name in file_versions:
            file_obj = FileModel()
            file_obj.fn = file_name
            file_obj.sha1 = hashlib.sha1(file_name.encode()).hexdigest()
            file_obj.size = 1
            file_obj.added_email = "example@example.com"
            file_obj.save()

            FileVersion.objects.create(
                file=file_obj,
                version_number=1
            )

        self.stdout.write(
            self.style.SUCCESS('Successfully created %s file versions' % len(file_versions))
        )
