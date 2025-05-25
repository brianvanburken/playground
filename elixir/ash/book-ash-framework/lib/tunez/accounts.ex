defmodule Tunez.Accounts do
  use Ash.Domain, otp_app: :tunez, extensions: [AshGraphql.Domain, AshJsonApi.Domain]

  graphql do
    mutations do
      create Tunez.Accounts.User, :register_user, :register_with_password
    end

    queries do
      get Tunez.Accounts.User, :sign_in_user, :sign_in_with_password do
        identity false
        type_name :user_with_token
      end
    end
  end

  json_api do
    routes do
      base_route "/users", Tunez.Accounts.User do
        post :register_with_password do
          route "/register"

          metadata fn _subject, user, _request ->
            %{token: user.__metadata__.token}
          end
        end

        post :sign_in_with_password do
          route "/sign-in"

          metadata fn _subject, user, _request ->
            %{token: user.__metadata__.token}
          end
        end
      end
    end
  end

  resources do
    resource Tunez.Accounts.Token

    resource Tunez.Accounts.User do
      define :set_user_role, action: :set_role, args: [:role]
      define :get_user_by_id, action: :read, args: [:id]
      define :get_user_by_email, action: :get_by_email, args: [:email]
    end

    resource Tunez.Accounts.Notification do
      define :notifications_for_user, action: :for_user
      define :dismiss_notification, action: :destroy
    end
  end
end
