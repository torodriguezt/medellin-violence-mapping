// map_rr_probabilidades_sexual.js

// En tu shapefile el nombre del departamento está en esta columna:
const DEPT_NAME_FIELD = "DPTO_CNMBR";

// Variables globales
let map;
let insetMap;
let geojsonData = null;
let csvRows = null;
let deptRR = {};
// We use fixed probability scale [0, 1]; RR min/max no longer needed
let rrMin = Infinity;
let rrMax = -Infinity;
let geojsonLayer = null;
let sanAndresLayer = null;

// Inicializar el mapa
function initMap() {
  map = L.map("map").setView([4.5, -74.1], 6);

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
    const val = parseFloat(row.RR);

    if (!isFinite(val)) return;

    // Filter strictly for grupo_bin == 1 (dynamicTyping converts it to number)
    if (row.grupo_bin === 1) {
      deptRR[nameKey] = row;

      // Keep RR min/max in case needed for popup context; not used for color now
      rrMin = Math.min(rrMin, val);
      rrMax = Math.max(rrMax, val);
    }
  });

  if (!isFinite(rrMin) || !isFinite(rrMax)) {
    rrMin = 0;
    rrMax = 1;
  }
}

// Paleta tipo viridis expandida con más pasos para mejor diferenciación en el rango alto
const viridisColors = [
  "#440154",  // 0.00 - morado oscuro
  "#482173",  // 0.05
  "#433e85",  // 0.10
  "#38598c",  // 0.15
  "#2d708e",  // 0.20
  "#25858e",  // 0.25
  "#1e9b8a",  // 0.30
  "#2ab07f",  // 0.35
  "#51c56a",  // 0.40
  "#85d54a",  // 0.45
  "#c2df23",  // 0.50
  "#fde725",  // 0.55
  "#fee825",  // 0.60
  "#fdd528",  // 0.65
  "#fcc029",  // 0.70
  "#fba72a",  // 0.75
  "#f98e2b",  // 0.80
  "#f6752c",  // 0.85
  "#f25c2d",  // 0.90
  "#ed432e",  // 0.95
  "#e8252f"   // 1.00 - rojo intenso
];

function getColorFromProb(p) {
  if (!isFinite(p)) return "#cccccc";
  // Clamp to [0,1]
  let prob = Math.max(0, Math.min(1, p));
  
  // Use piecewise linear transformation to emphasize high probability range
  // This gives more color steps to probabilities above 0.75
  let t;
  if (prob < 0.75) {
    // Map 0.00-0.75 to 0.0-0.5 (first half of colors)
    t = prob / 1.5;
  } else {
    // Map 0.75-1.00 to 0.5-1.0 (second half of colors)
    t = 0.5 + (prob - 0.75) * 2;
  }
  
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
  updateStatus("Data loaded. Drawing map...");

  // Función de estilo común (color by probability P(RR > 1.25))
  const getStyle = (feature) => {
    const rawName = feature.properties[DEPT_NAME_FIELD];
    const nameKey = normalizeName(rawName);
    const row = deptRR[nameKey];
    const prob = row ? parseFloat(row.prob_rr_gt1_2) : NaN;

    return {
      fillColor: getColorFromProb(prob),
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
      const val = row ? parseFloat(row.RR) : NaN;
      const y = row ? row.y : null;
      const E = row ? row.E : null;
      const prob = row ? parseFloat(row.prob_rr_gt1_2) : null;

      let html = `<strong>${rawName || "No name"}</strong><br>`;
      if (isFinite(val)) {
        html += `RR: <strong>${val.toFixed(3)}</strong><br>`;
      } else {
        html += `RR: N/A<br>`;
      }
      if (y != null && E != null) {
        html += `Observed cases: ${y}<br>Expected cases: ${E.toFixed(2)}<br>`;
      }
      if (prob != null && isFinite(prob)) {
        html += `P(RR > 1.25): ${(prob * 100).toFixed(2)}%<br>`;
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
    insetMap.fitBounds(bounds, { padding: [10, 10], maxZoom: 20 });
  }

  addLegend();
  addSanAndresControl();
}

// Leyenda
function addLegend() {
  const legend = L.control({ position: "bottomright" });

  legend.onAdd = function () {
    const div = L.DomUtil.create("div", "info legend");
    
    div.innerHTML = '<div class="title" style="margin-bottom: 8px; font-size: 22px; font-weight: bold;">Probability P(RR > 1.25)</div>';
    
    // Create continuous gradient bar with more intermediate values emphasizing high range
    let gradientHTML = '<div style="display: flex; align-items: stretch;">';
    gradientHTML += '<div style="display: flex; flex-direction: column; justify-content: space-between; margin-right: 12px; font-size: 20px; font-weight: 600;">';
    gradientHTML += `<div style="font-size: 22px;">1.00</div>`;
    gradientHTML += `<div style="color: #333;">0.95</div>`;
    gradientHTML += `<div style="color: #333;">0.90</div>`;
    gradientHTML += `<div style="color: #333;">0.85</div>`;
    gradientHTML += `<div style="color: #333;">0.80</div>`;
    gradientHTML += `<div style="color: #555;">0.75</div>`;
    gradientHTML += `<div style="color: #666;">0.50</div>`;
    gradientHTML += `<div style="color: #666;">0.25</div>`;
    gradientHTML += `<div style="font-size: 22px;">0.00</div>`;
    gradientHTML += '</div>';
    gradientHTML += '<div style="background: linear-gradient(to bottom, ';
    for (let i = viridisColors.length - 1; i >= 0; i--) {
      gradientHTML += viridisColors[i];
      if (i > 0) gradientHTML += ', ';
    }
    gradientHTML += '); width: 60px; height: 300px;"></div>';
    gradientHTML += '</div>';
    
    div.innerHTML += gradientHTML;
    
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
    btnSA.title = "Go to San Andrés";
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
  updateStatus("Loading data...");

  Promise.all([
    // Cargar shapefile zipeado
    shp("../departamentos.zip"),
    // Cargar CSV
    new Promise((resolve, reject) => {
      Papa.parse("../resultados/dat_inla_resultados_con_probabilidades.csv", {
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
    console.error("Error loading data:", err);
    updateStatus("Error loading data: " + err.message);
  });
}

// Inicialización
document.addEventListener("DOMContentLoaded", () => {
  initMap();
  loadData();
});
