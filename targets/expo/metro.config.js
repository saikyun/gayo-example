const defaultAssetExts = require("metro-config/src/defaults/defaults").assetExts;
const path = require("path");

module.exports = {
    transformer: {
        getTransformOptions: async () => ({
            transform: {
                experimentalImportSupport: false,
                inlineRequires: false,
            },
        }),
    },
    resolver: {
        assetExts: [
            ...defaultAssetExts, // <- array spreading defaults
          'glb',
	  'gltf',
	  'bin',
	  'fnt',
	  'mp3'
        ]
    },
    watchFolders: [
        path.resolve(__dirname, "../../assets")
    ]
};
