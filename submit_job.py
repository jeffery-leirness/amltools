def submit_job(code = ".", rscript_command = "", dir_out = None, dir_env = None, compute = "", experiment_name = "", display_name = "", description = ""
):
  
  # import required libraries
  from azure.ai.ml import MLClient
  from azure.ai.ml import command, Input, Output
  from azure.ai.ml.entities import Environment, Data, BuildContext
  from azure.identity import DefaultAzureCredential

  # Enter details of your AML workspace
  subscription_id = "737b86ee-60d4-40ce-bb2f-11f4ef6f4f8c"
  resource_group = "nccos-rg-machinelearning-prod-e2"
  workspace = "nccos-ws-dev-e2"

  # get a handle to the workspace
  ml_client = MLClient(
    DefaultAzureCredential(), subscription_id, resource_group, workspace
  )

  # configure the command
  if dir_out is None:
    outputs = None
  else:
    outputs = {"dir_out": Output(type = "uri_folder", path = dir_out, mode = "rw_mount")}
  if dir_env is None:
    environment = None
  else:
    environment = Environment(build = BuildContext(path = dir_env))
  job = command(
    code = code,
    command = rscript_command,
    outputs = outputs,
    environment = environment,
    compute = compute,
    experiment_name = experiment_name,
    display_name = display_name,
    description = description
  )

  # submit the command
  ml_client.create_or_update(job)
