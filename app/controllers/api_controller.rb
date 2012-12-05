class ApiController < ApplicationController
  before_filter :authenticate_account!
  skip_before_filter :verify_authenticity_token

  def errors_to_json(model_object, action)
    attrs = {
      :summary => I18n.t("controllers.api_controller.problem_action"),
      :properties => []
    }
    model_object.errors.each do |name, value|
      attrs[:properties] << { name => value }
    end
    attrs
  end
end