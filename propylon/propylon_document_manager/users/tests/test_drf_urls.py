from django.urls import resolve, reverse

from propylon_document_manager.users.models import User


def test_user_detail(user: User):
    assert reverse("users:detail", kwargs={"pk": user.pk}) == f"/users/{user.pk}/"
    assert resolve(f"/users/{user.pk}/").view_name == "users:detail"


def test_user_list():
    assert reverse("users:user-list") == "/users/user-list/"
