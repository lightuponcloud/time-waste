from collections.abc import Sequence
from typing import Any

from factory import Faker, post_generation
from factory.django import DjangoModelFactory


class FileModelFactory(DjangoModelFactory):
    fn = Faker("fn")
    sha1 = Faker("sha1")
    size = Faker("size")

    class Meta:
        django_get_or_create = ["id"]
