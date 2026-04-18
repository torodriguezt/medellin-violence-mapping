// map.js

// En tu shapefile el nombre del departamento está en esta columna:
const DEPT_NAME_FIELD = "DPTO_CNMBR";

// Variables globales
let map;
let insetMap;
let geojsonData = null;
let csvRows = null;
let deptRR = {};
let rrMin = Infinity;
let rrMax = -Infinity;
let geojsonLayer = null;
let sanAndresLayer = null;

// Inicializar el mapa
function initMap() {
  map = L.map("map").setView([4.5, -74.1], 5);

  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19,
    attribution: '&copy; OpenStreetMap contributors'
  }).addTo(map);

  // Inicializar mapa inset (San Andrés)
  insetMap = L.map("map-inset", {
    zoomControl: false,
    attributionControl: false,
    dragging: false,
    scrollWheelZoom: false,
    doubleClickZoom: false,
    boxZoom: false,
    touchZoom: false,
    keyboard: false
  }).setView([12.55, -81.7], 10);

  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    maxZoom: 19
  }).addTo(insetMap);
}

// Normalizador de nombres, para que coincida shapefile <-> CSV
function normalizeName(str) {
  if (!str) return "";
  let s = str
    .toString()
    .toLowerCase()
    .normalize("NFD")
    .replace(/[\u0300-\u036f]/g, "") // quita acentos
    .replace(/[^\w\s]/g, " ")        // quita signos
    .replace(/\s+/g, " ")
    .trim();

  // Ajustes específicos para que calce con tu depto_norm
  if (s.includes("archipielago de san andres")) {
    s = "san andres";
  }
  if (s.includes("san andres")) {
    s = "san andres";
  }
  if (s.includes("bogota")) {
    s = "bogota dc";
  }

  return s;
}

// Construir diccionario depto -> fila CSV (preferimos grupo_bin == 0)
function buildDeptRR() {
  deptRR = {};
  rrMin = Infinity;
  rrMax = -Infinity;

  csvRows.forEach((row) => {
    const nameKey = normalizeName(row.depto_norm);
    const rr = parseFloat(row.RR);

    if (!isFinite(rr)) return;

    const grupo = row.grupo_bin !== undefined ? String(row.grupo_bin) : null;
    if (!deptRR[nameKey] || grupo === "0") {
      deptRR[nameKey] = row;
    }

    rrMin = Math.min(rrMin, rr);
    rrMax = Math.max(rrMax, rr);
  });

  if (!isFinite(rrMin) || !isFinite(rrMax)) {
    rrMin = 0;
    rrMax = 1;
  }
}

// Paleta tipo viridis
const viridisColors = [
  "#440154",
  "#3b528b",
  "#21908d",
  "#5dc863",
  "#fde725"
];

function getColorFromRR(rr) {
  if (!isFinite(rr)) return "#cccccc";
  const t = (rr - rrMin) / (rrMax - rrMin || 1);
  const idx = Math.max(
    0,
    Math.min(viridisColors.length - 1, Math.floor(t * (viridisColors.length - 1)))
  );
  return viridisColors[idx];
}

// Dibujar capa GeoJSON
function drawGeoJSON() {
  if (!map || !geojsonData || !csvRows) return;

  if (geojsonLayer) {
    map.removeLayer(geojsonLayer);
  }

  buildDeptRR();
  updateStatus("Datos cargados. Dibujando mapa...");

  // Función de estilo común
  const getStyle = (feature) => {
    const rawName = feature.properties[DEPT_NAME_FIELD];
    const nameKey = normalizeName(rawName);
    const row = deptRR[nameKey];
    const rr = row ? parseFloat(row.RR) : NaN;

    return {
      fillColor: getColorFromRR(rr),
      fillOpacity: 0.85,
      color: "#374151",
      weight: 1
    };
  };

  // Capa principal
  geojsonLayer = L.geoJSON(geojsonData, {
    style: getStyle,
    onEachFeature: (feature, layer) => {
      const rawName = feature.properties[DEPT_NAME_FIELD];
      const nameKey = normalizeName(rawName);
      const row = deptRR[nameKey];
      const rr = row ? parseFloat(row.RR) : NaN;
      const y = row ? row.y : null;
      const E = row ? row.E : null;

      let html = `<strong>${rawName || "Sin nombre"}</strong><br>`;
      if (isFinite(rr)) {
        html += `RR: <strong>${rr.toFixed(3)}</strong><br>`;
      } else {
        html += `RR: N/A<br>`;
      }
      if (y != null && E != null) {
        html += `Casos observados: ${y}<br>Casos esperados: ${E}<br>`;
      }

      layer.bindPopup(html);

      const key = normalizeName(rawName);
      if (key.includes("san andres")) {
        sanAndresLayer = layer;
      }
    }
  }).addTo(map);

  // Capa inset (solo visual)
  L.geoJSON(geojsonData, {
    style: getStyle
  }).addTo(insetMap);

  map.fitBounds(geojsonLayer.getBounds(), { padding: [20, 20] });

  // Centrar inset en San Andrés si se encontró
  if (sanAndresLayer) {
    const bounds = sanAndresLayer.getBounds();
    insetMap.fitBounds(bounds, { padding: [10, 10], maxZoom: 18 });
  }

  addLegend();
  addSanAndresControl();
}

// Leyenda
function addLegend() {
  const legend = L.control({ position: "bottomright" });

  legend.onAdd = function () {
    const div = L.DomUtil.create("div", "info legend");
    const grades = 5;
    const step = (rrMax - rrMin || 1) / grades;

    let labels = [];
    div.innerHTML = '<div class="title">RR</div>';

    for (let i = 0; i < grades; i++) {
      const from = rrMin + step * i;
      const to = i === grades - 1 ? rrMax : rrMin + step * (i + 1);
      const color = getColorFromRR((from + to) / 2);
      labels.push(
        `<i style="background:${color}"></i> ${from.toFixed(2)} &ndash; ${to.toFixed(2)}`
      );
    }

    div.innerHTML += labels.join("<br>");
    return div;
  };

  legend.addTo(map);
}

// Control de zoom específico para San Andrés
function addSanAndresControl() {
  if (!sanAndresLayer) return;

  const control = L.control({ position: "topleft" });

  control.onAdd = function () {
    const container = L.DomUtil.create("div", "leaflet-bar leaflet-control");
    
    // Botón San Andrés
    const btnSA = L.DomUtil.create("a", "", container);
    btnSA.innerHTML = "🏝️ San Andrés";
    btnSA.href = "#";
    btnSA.title = "Ir a San Andrés";
    btnSA.style.display = "block";
    btnSA.style.padding = "5px 10px";
    btnSA.style.textDecoration = "none";
    btnSA.style.color = "black";
    btnSA.style.backgroundColor = "white";
    btnSA.style.fontSize = "12px";
    btnSA.style.fontWeight = "bold";

    L.DomEvent.on(btnSA, "click", (e) => {
      L.DomEvent.stop(e);
      const center = sanAndresLayer.getBounds().getCenter();
      map.setView(center, 11);
      sanAndresLayer.openPopup();
    });

    // Botón Reset
    const btnReset = L.DomUtil.create("a", "", container);
    btnReset.innerHTML = "🇨🇴 Colombia";
    btnReset.href = "#";
    btnReset.title = "Ver todo el país";
    btnReset.style.display = "block";
    btnReset.style.padding = "5px 10px";
    btnReset.style.textDecoration = "none";
    btnReset.style.color = "black";
    btnReset.style.backgroundColor = "white";
    btnReset.style.borderTop = "1px solid #ccc";
    btnReset.style.fontSize = "12px";
    btnReset.style.fontWeight = "bold";

    L.DomEvent.on(btnReset, "click", (e) => {
      L.DomEvent.stop(e);
      if (geojsonLayer) {
        map.fitBounds(geojsonLayer.getBounds(), { padding: [20, 20] });
      }
    });

    return container;
  };

  control.addTo(map);
}

// Status
function updateStatus(msg) {
  const el = document.getElementById("status");
  if (el) el.textContent = msg;
}

// Cargar datos automáticamente (Shapefile zip y CSV)
function loadData() {
  updateStatus("Cargando datos...");

  Promise.all([
    // Cargar shapefile zipeado
    shp("departamentos.zip"),
    // Cargar CSV
    new Promise((resolve, reject) => {
      Papa.parse("dat_inla_resultados.csv", {
        download: true,
        header: true,
        dynamicTyping: true,
        skipEmptyLines: true,
        complete: (results) => resolve(results.data),
        error: (err) => reject(err)
      });
    })
  ]).then(([geojson, csvData]) => {
    geojsonData = geojson;
    csvRows = csvData;
    updateStatus("Datos cargados. Dibujando mapa...");
    drawGeoJSON();
  }).catch(err => {
    console.error("Error cargando datos:", err);
    updateStatus("Error cargando datos: " + err.message);
  });
}

// Inicialización
document.addEventListener("DOMContentLoaded", () => {
  initMap();
  loadData();
});
