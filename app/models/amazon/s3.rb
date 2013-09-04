module Amazon
  class S3
    CONFIG_FILE_PATH = "#{Rails.root}/config/aws.yml"
    BUCKET_NAME = 'verboice-cambodia'

    def initialize
      @s3 = AWS::S3.new
    end

    def write_to_server file
      if file
        p "=============== uploading to amazon s3 ==============="
        @key = File.basename(file)
        @bucket = @s3.buckets[BUCKET_NAME]
        @object = @bucket.objects[@key]
        @object.write Pathname.new(file)
        p "=============== done ==============="
      else
        raise "file can't be null"
      end
    end

    def self.upload file
      instance = Amazon::S3.new
      instance.write_to_server file
    end

  end
end