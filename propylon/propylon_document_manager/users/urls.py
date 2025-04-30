from django.urls import path

from propylon_document_manager.users.views import (
    user_detail_view,
    user_redirect_view,
    user_update_view,
    users_list_view
)

app_name = "users"
urlpatterns = [
    path("~redirect/", view=user_redirect_view, name="redirect"),
    path("~update/", view=user_update_view, name="update"),
    path("user-list/", view=users_list_view, name="user-list"),
    path("<int:pk>/", view=user_detail_view, name="detail"),
]
