const box = {
  locked: true,
  unlock() { this.locked = false; },
  lock() { this.locked = true;  },
  _content: [],
  get content() {
    if (this.locked) throw new Error("Locked!");
    return this._content;
  }
};

function withBoxUnlocked(body) {
  const wasLocked = box.locked
  box.unlock()
  try {
    body()
  } finally {
    if(wasLocked) {
      box.lock()
    }
  }
}
