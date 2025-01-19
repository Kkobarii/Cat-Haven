let username = '';
let selectedCatId = null;
let randomCatMode = false;
const apiBase = 'http://localhost:3000';

function formatTimestamp(timestamp) {
  const date = new Date(timestamp);
  return date.toLocaleString();
}

// Initialize App
document.getElementById('start-btn').addEventListener('click', () => {
  username = document.getElementById('username').value.trim();
  if (!username) {
    alert('Please enter a username.');
    return;
  }

  document.getElementById('username-section').classList.add('d-none');
  document.getElementById('app').classList.remove('d-none');
  document.getElementById('username-display').innerText = username;
  loadGallery();
});

// Load Gallery
async function loadGallery() {
  const response = await fetch(`${apiBase}/cats`);
  const cats = await response.json();

  const gallery = document.getElementById('gallery');
  gallery.innerHTML = '';

  cats.forEach(cat => {
    const img = document.createElement('img');
    img.src = cat.url;
    img.alt = cat.id;
    img.dataset.id = cat.id;
    img.dataset.owner = cat.user;
    img.addEventListener('click', () => toggleGalleryCatDetail(cat));
    gallery.appendChild(img);
  });
}

// Load Comments for a Cat
async function loadComments(catId) {
  const response = await fetch(`${apiBase}/cats/${catId}/comments`);
  const comments = await response.json();

  const commentsList = document.getElementById('comments-list');
  commentsList.innerHTML = '';
  comments.forEach(comment => {
    const li = document.createElement('li');
    li.classList.add('comment-item');

    const userAndTimestamp = document.createElement('div');
    userAndTimestamp.classList.add('comment-header');
    userAndTimestamp.innerHTML = `<strong>${comment.user}</strong> <span class="comment-timestamp">${formatTimestamp(comment.timestamp)}</span>`;
    
    const commentText = document.createElement('div');
    commentText.classList.add('comment-text');
    commentText.innerText = comment.text;

    li.appendChild(userAndTimestamp);
    li.appendChild(commentText);
    commentsList.appendChild(li);
  });
}

// Toggle Gallery Cat Detail
async function toggleGalleryCatDetail(cat) {
  if (selectedCatId === cat.id) {
    document.getElementById('detail-section').classList.add('d-none');
    selectedCatId = null;
    return;
  }

  selectedCatId = cat.id;
  randomCatMode = false;

  document.getElementById('detail-image').src = cat.url;
  document.getElementById('detail-owner').innerText = cat.user;

  await loadComments(cat.id);

  document.getElementById('detail-section').classList.remove('d-none');
  document.getElementById('random-buttons').classList.add('d-none');
  document.getElementById('gallery-details').classList.remove('d-none');

  const currentUser = document.getElementById('username').value;
  const deleteButton = document.getElementById('delete-btn');
  
  if (cat.user === currentUser) {
    deleteButton.classList.remove('d-none');
  } else {
    deleteButton.classList.add('d-none');
  }
}

// Get a Random Cat
let randomCat = null;
async function getRandomCat() {
  const response = await fetch(`${apiBase}/cats/random`);
  const cat = await response.json();

  randomCat = {
    id: cat.id,
    url: cat.url,
    width: cat.width,
    height: cat.height,
  };

  document.getElementById('detail-image').src = randomCat.url;
  document.getElementById('detail-section').classList.remove('d-none');
  document.getElementById('random-buttons').classList.remove('d-none');
  document.getElementById('gallery-details').classList.add('d-none');
}

// Save Random Cat
async function saveRandomCat() {
  if (!randomCat) {
    alert('No random cat to save!');
    return;
  }

  const response = await fetch(`${apiBase}/cats`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      user: username,
      cat: randomCat,
    }),
  });

  const result = await response.text();

  if (result === '"Cat already exists"') {
    alert('This cat is already in the gallery!');
  } else if (result === '"Cat saved successfully!"') {
    alert('Cat saved successfully!');
    document.getElementById('detail-section').classList.add('d-none');
    randomCat = null;
    loadGallery();
  } else {
    alert('An unexpected error occurred while saving the cat.');
  }
}

// Add Comment
document.getElementById('comment-btn').addEventListener('click', async () => {
  const commentText = document.getElementById('comment-input').value.trim();
  if (!commentText) {
    alert('Please enter a comment.');
    return;
  }

  await fetch(`${apiBase}/cats/${selectedCatId}/comments`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ user: username, comment: commentText }),
  });

  document.getElementById('comment-input').value = '';
  await loadComments(selectedCatId);
});

// Delete Cat
document.getElementById('delete-btn').addEventListener('click', async () => {
  const response = await fetch(`${apiBase}/cats/${selectedCatId}`, {
    method: 'DELETE',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ user: username }),
  });

  const result = await response.text();
  console.log(result);

  if (result === '"User not authorized to delete this cat"') {
    alert('You are not authorized to delete this cat.');
  } else if (result === '"Cat not found"') {
    alert('This cat no longer exists.');
  } else if (result === '"Cat deleted successfully!"') {
    alert('Cat deleted successfully!');
    document.getElementById('detail-section').classList.add('d-none');
    selectedCatId = null;
    loadGallery();
  } else {
    alert('An unexpected error occurred.');
  }
});

// Cancel Detail View
document.getElementById('cancel-btn').addEventListener('click', () => {
  document.getElementById('detail-section').classList.add('d-none');
  selectedCatId = null;
});

// Get Random Cat
document.getElementById('random-cat-btn').addEventListener('click', getRandomCat);

// Save Random Cat Button
document.getElementById('save-btn').addEventListener('click', saveRandomCat);
