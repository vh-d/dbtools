#' Verify that Oracle drive is available and sets necessary environemtn variables (Windows only)
#' sets env variables for Oracle driver on Windows (64-bit)
#' @param bin_path path to Oracle binaries (such as "C:/ORACLE/11.2.0/bin")
#' @param oci_path path to Oracle binaries (such as "C:/ORACLE/11.2.0/oci/include")
#' @export
setUpOracle <- function(bin_path,
                        oci_path) {

  # check that powershell is present on the system
  err <- suppressWarnings(system(command = "where powershell.exe", show.output.on.console = FALSE))
  if (err != 0) stop("Cannot locate powershell.exe")

  # verify Oracle headers - probably only necessary for compilation of ROracle?
  env1 <- Sys.getenv("OCI_INC")

  if (env1 == "") {
    if (file.exists(oci_path)) {
      shell(cmd   = str_interp("[Environment]::SetEnvironmentVariable('OCI_INC', '${oci_path}', 'User')"),
            shell = "powershell.exe")
    } else {
      warning("Cannot find OCI headers at ", oci_path)
    }
  } else {
    warning("OCI_INC environment variable is already set to: ", env1, " ... skipping ...")
  }

  # verify Oracle binaries
  env2 <- Sys.getenv("OCI_LIB64")

  if (env2 == "") {
    if (file.exists(bin_path)) {
      shell(cmd   = str_interp("[Environment]::SetEnvironmentVariable('OCI_LIB64', '${bin_path}', 'User')"),
            shell = "powershell.exe")
    } else {
      warning("Cannot find Oracle binary at ", bin_path)
    }
  } else {
    warning("OCI_LIB64 environment variable is already set to: ", env2, " ... skipping ...")
  }

}
