require 'aws-sdk-lambda'
require 'zip/zip'
require 'base64'

class Lambda
  def hello
    # create the zip in memory
    # https://gist.github.com/ampedandwired/3385002
    dir = 'greetingsOnDemand/'
    file_stream = Zip::ZipOutputStream.write_buffer do |zipfile|
      Dir[File.join(dir, '**/*')].each do |file|
        path_for_file_in_zip = file.sub(/\A#{dir}\//, '')
        if !File.directory?(file)
          zip_entry = zipfile.put_next_entry(path_for_file_in_zip)
          zipfile << IO.read(file)
        end
      end
    end

    # encode the zip stream to Base64
    encoded_stream = Base64.encode64(file_stream.string)

    # create new Lambda client
    client = Aws::Lambda::Client.new(
      region: 'eu-central-1',
      access_key_id: ENV['ACCESS_KEY'],
      secret_access_key: ENV['ACCESS_SECRET']
    )

    # upload function
    resp = client.create_function({
      function_name: 'greetingsOnDemand',
      runtime: 'nodejs8.10',
      role: ENV['ROLE'],
      handler: 'greetingsOnDemand.handler',
      code: {
        zip_file: encoded_stream
      },
      environment: {
        variables: {
          GREETING: "Hola"
        }
      }
    })

    resp.to_h
  end
end

i = Lambda.new
puts i.hello
