require 'aws-sdk-iam'

class IAM
  def hello
    creds = Aws.config[:credentials] = Aws::Credentials.new(
      ENV['ACCESS_KEY'], ENV['ACCESS_SECRET']
    )
    iam = Aws::IAM::Client.new
    puts iam.list_roles.map(&:arn)
    nil
  end
end

i = IAM.new
i.hello
