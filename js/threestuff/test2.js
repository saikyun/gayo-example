import { Platform } from '@unimodules/core';
import { readAsStringAsync } from 'expo-file-system';

import * as THREE from 'three';

export async function readFromFileSystemAsStringAsync(
  localUri
) {
  if (Platform.OS === 'web') {
    const loader = new THREE.FileLoader();
    return new Promise((resolve, reject) =>
      loader.load(
        localUri,
        async value => {
          // @ts-ignore
          resolve(await value);
        },
        () => {},
        reject,
      ),
    );
  }
  try {
    return await readAsStringAsync(localUri);
  } catch ({ message }) {
    throw new Error(
      `ExpoTHREE: FileSystem.readAsStringAsync(${localUri}) ${message}`,
    );
  }
}
