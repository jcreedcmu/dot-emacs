#!/usr/bin/env node

// usage:
// ./change-workspace.js 1 # move this workspace left
// ./change-workspace.js -1 # move this workspace right
MIN_WORKSPACE = 1;
MAX_WORKSPACE = 6;
const util = require('util');
const exec = util.promisify(require('child_process').execFile);

async function get_workspaces() {
  const { stdout, stderr } = await exec('i3-msg', ['-t', 'get_workspaces']);
  if (stderr.length > 0) {
	 throw new Error(stderr);
  }
  return JSON.parse(stdout);
}

async function get_current_workspace() {
  const workspaces = await get_workspaces(); // XXX cache
  return workspaces.find(w => w.focused).num;
}

async function go(delta) {
  const cws = await get_current_workspace();
  const nws = cws + delta;

  if (nws < MIN_WORKSPACE || nws > MAX_WORKSPACE)
	 return;

  await exec('i3-msg', ['rename', 'workspace', cws, 'to', 'temp']);
  await exec('i3-msg', ['rename', 'workspace', nws, 'to', cws]);
  await exec('i3-msg', ['rename', 'workspace', 'temp', 'to', nws]);
}

go(parseInt(process.argv[2] ?? 0));
