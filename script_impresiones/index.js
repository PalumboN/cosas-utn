const fs = require('fs')

const INPUT_DIR = "/home/nahue/git/pdep/2019-l-parcialDakar/"
const OUTPUT_DIR = "/home/nahue/Desktop/correcciones/"
const SOLUCION = "dakar.pl"

const errores = []

function renombrarYJuntarSolucion(entrega) {
    const dir = INPUT_DIR + entrega + "/"
    if (!fs.existsSync(dir + SOLUCION)) {
        errores.push('Solution not found: ' + entrega);
        return
    }

    const nuevaSolucion = entrega + ".txt" 

    console.log("Renombrando solución:", entrega)
    fs.renameSync(dir + SOLUCION, dir + nuevaSolucion)
    console.log("OK")

    console.log("Copiando solución:", entrega)
    fs.copyFileSync(dir + nuevaSolucion, OUTPUT_DIR + nuevaSolucion)
    console.log("OK")
}

////////////////////////////

const entregas = fs.readdirSync(INPUT_DIR)

entregas.forEach(renombrarYJuntarSolucion)

console.log("Reporte de errores:")
errores.forEach(e => console.log(e))