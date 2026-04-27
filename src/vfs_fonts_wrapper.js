// ES module wrapper for vfs_fonts
import vfsFontsRaw from './vfs_fonts.js?url'

// Create a global object for pdfMake
const pdfMakeGlobal = {}

// Execute the vfs_fonts script in a controlled context
const script = document.createElement('script')
script.src = vfsFontsRaw
document.head.appendChild(script)

// Export the pdfMake object after it's populated
export default pdfMakeGlobal
