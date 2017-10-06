require "azure"
require "optparse"

## Resources:

## Starting tips:
## https://docs.microsoft.com/en-us/azure/storage/blobs/storage-ruby-how-to-use-blob-storage

## Doc for Azure on Ruby
## http://www.rubydoc.info/github/yaxia/azure-storage-ruby/Azure

## gli docs
## https://github.com/davetron5000/gli

## Ruby doc
## https://ruby-doc.org/core-2.1.4/index.html
## http://langref.org/all-languages/lists/access/fetch-by-index

## Example of command-line application in Ruby
## http://blog.honeybadger.io/writing-command-line-apps-in-ruby/

## Run examples:
## 1. list content of container (in this case container "lightstructures"):
## $ ruby azure.rb list lightstructures
##
## 2. upload file ~/mydata/test.csv to container:
## $ ruby azure.rb upload lightstructures ~/mydata/test.csv
##
## 3. download file from container (not implemented!)
## $ ruby azure.rb download lightstructures test.csv
## $ ruby azure.rb download lightstructures -o ~/tmp/localtest.csv test.csv
##
## 4. delete file from container (not implemented!)
## $ ruby azure.rb delete lightstructures test.csv


## These values may also be stored in environment variables
## AZURE_STORAGE_ACCOUNT and AZURE_STORAGE_ACCESS_KEY
Azure.config.storage_account_name = "nosdev"
Azure.config.storage_access_key = "DP4UrYghy/vL4V5ZScDh9w65ms38XLAOQw1LsMauQVONzxwllfVuRR/KrC4pNEfefjQbSzDqJg/KkXLGovN9sQ=="

## Global variable
$azure_blob_service = Azure::Blob::BlobService.new

## Get the 'lightstructures' container as an object
def get_container(container_name = "lightstructures")
  containers = $azure_blob_service.list_containers()
  return containers.detect { |c| c.name == container_name } # Must be better ways?!
end

## Return a list of all blobs in container
def get_blobs(container)
  return $azure_blob_service.list_blobs(container.name)
end

## List all blobs in 'lightstructures'
def list_blobs(blobs)
  blobs.each do |blob|
    puts blob.name
  end
end

def upload_file(path, container, blob_name = "")
 content = File.open(path, "rt") { |file| file.read }
 blob = $azure_blob_service.create_block_blob(container.name, blob_name, content)
end

require 'gli'
include GLI::App
program_desc 'Easy command-line interface to Azure'

subcommand_option_handling :normal
arguments :strict

desc 'Describe some switch here'
switch [:s,:switch]

desc 'Describe some flag here'
default_value 'the default'
arg_name 'The name of the argument'
flag [:f,:flagname]

desc 'List the contents of container'
default_value 'lightstructures'
arg_name 'container'
command :list do |c|
  c.desc 'List the contents of the container at URL'
  c.switch :s

  c.desc 'Describe a flag to list'
  c.default_value 'default'
  c.flag :f
  c.action do |global_options,options,args|

    # Your command logic here
     
    # If you have any errors, just raise them
    # raise "that command made no sense"
    ls = get_container(args.at(0))
    list_blobs(get_blobs(ls))
  end
end

desc 'Download blob from container'
arg_name 'container blob_name [destination]'
command :download do |c|
  c.action do |global_options,options,args|
    ls = get_container(args.at(0))
    blob_name = get_container(args.at(1))
    path = get_container(args.at(2))
    raise "Download blob from container is not implemented"
  end
end

desc 'Upload file at target to container as a block blob'
arg_name 'container target [blob_name]'
command :upload do |c|
  c.action do |global_options,options,args|
    ls = get_container(args.at(0))
    path = args.at(1)
    blob_name = args.at(2)
    upload_file(path, ls, blob_name)
    puts "Uploaded " + path + " to container " + ls.name + " as blob " + blob_name
  end
end

desc 'Delete blob from container'
arg_name 'Describe arguments to add here'
command :delete do |c|
  c.action do |global_options,options,args|
    raise "Delete blob from container is not implemented"
  end
end

pre do |global,command,options,args|
  # Pre logic here
  # Return true to proceed; false to abort and not call the
  # chosen command
  # Use skips_pre before a command to skip this block
  # on that command only
  true
end

post do |global,command,options,args|
  # Post logic here
  # Use skips_post before a command to skip this
  # block on that command only
end

on_error do |exception|
  # Error logic here
  # return false to skip default error handling
  true
end

exit run(ARGV)
