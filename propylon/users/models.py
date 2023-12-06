from django.contrib.auth.models import AbstractUser
from django.db.models import CharField, EmailField
from django.db import models
from django.urls import reverse
from django.utils.translation import gettext_lazy as _

from propylon_document_manager.users.managers import UserManager


class User(AbstractUser):
    """
    Default custom user model for Propylon Document Manager.
    If adding fields that need to be filled at user signup,
    check forms.SignupForm and forms.SocialSignupForms accordingly.
    """

    # First and last name do not cover name patterns around the globe
    name = CharField(_("Name of User"), blank=True, max_length=255)
    first_name = None  # type: ignore
    last_name = None  # type: ignore
    email = EmailField(_("email address"), unique=True)
    username = None  # type: ignore

    USERNAME_FIELD = "email"
    REQUIRED_FIELDS = []

    objects = UserManager()

    def get_absolute_url(self) -> str:
        """Get URL for user's detail view.

        Returns:
            str: URL for user detail.

        """
        return reverse("users:detail", kwargs={"pk": self.id})


class BaseRestrictedModel(models.Model):
    """
    Defines methods and attributes used for permission checks
    """
    class Meta:
        abstract = True

    added_email = models.CharField("Who Added", max_length=75)

    def _get_added_by(self):
        try:
            user = User.objects.get(email=self.added_email)
        except User.DoesNotExist:
            name = self.added_email
        else:
            name = user.email
        return name

    def _set_added_by(self, user):
        self.added_email = user.email

    added_by = property(_get_added_by, _set_added_by)

    def _get_added_by_user(self):
        try:
            user = User.objects.get(email=self.added_email)
        except User.DoesNotExist:
            return None
        except User.MultipleObjectsReturned:
            return User.objects.filter(email=self.added_email)[0]
        else:
            return user
    added_by_user = property(_get_added_by_user)

    def can_read(self, email):
        try:
            user = User.objects.get(email=email)
        except User.DoesNotExist:
            return False
        else:
            if self.added_email == email:
                return True
        return False
