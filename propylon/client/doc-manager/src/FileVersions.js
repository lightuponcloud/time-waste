import React, { useState, useEffect } from "react";

import "./FileVersions.css";

function FileVersionsList(props) {
  const file_versions = props.file_versions;
  return file_versions.map((file_version) => (
    <div className="file-version" key={file_version.id}>
      <h2>File Name: {file_version.file_name}</h2>
      <p>
        ID: {file_version.id} Version: {file_version.version_number}
      </p>
    </div>
  ));
}
function FileVersions() {
  const [data, setData] = useState([]);
  console.log(data);

  useEffect(() => {
    // fetch data
    const dataFetch = async () => {
      const data = await (
        await fetch("http://localhost:8001/api/file_versions/")
      ).json();

      // set state when the data received
      setData(data);
    };

    dataFetch();
  }, []);
  return (
    <div>
      <h1>Found {data.length} File Versions</h1>
      <div>
        <FileVersionsList file_versions={data} />
      </div>
    </div>
  );
}

export default FileVersions;
