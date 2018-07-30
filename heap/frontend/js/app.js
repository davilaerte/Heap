function request(uri, body) {
    return fetch(`http://localhost:3000${uri}`, {
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        method: "POST",
        body: JSON.stringify(body)
    }).then(response => response.json());
}

function buildHeap() {
    const elements = form.elements.value;
    buildAux(elements.split(','));
    return false;
}

function buildAux(heap_elements) {
    request('/build', {
        heapLis: heap_elements.map(element => Number(element)),
        comp: comp
    }).then(data => {
        heap = data.heap;
        atualizeHeap();
    });
}

function atualizeHeap() {
    const generated = document.getElementById('generated');
    generated.innerText = `HEAP: [${heap}]`;
}

function insert() {
    const element = insert_ele.element.value;
    request(`/insert/${element}`, {
        heapLis: heap.map(element => Number(element)),
        comp: comp
    }).then(data => {
        heap = data.heap;
        atualizeHeap();
    });
}

function extract() {
    const ext = document.getElementById('extract');

    request('/extract', {
        heapLis: heap.map(element => Number(element)),
        comp: comp
    }).then(data => {
        ext.innerText = "Elemento extraído: " + data[0];
        heap = data[1].heap;
        atualizeHeap();
    });
}

const form = document.getElementById('build');
const insert_ele = document.getElementById('insert');
const sel = document.getElementById('heap-type');
let heap = [];
let comp = true;
sel.onchange = () => { comp = Boolean(sel.value); buildAux(heap); };

