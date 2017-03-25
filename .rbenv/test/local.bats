#!/usr/bin/env bats

load test_helper

setup() {
  mkdir -p "${RBENV_TEST_DIR}/myproject"
  cd "${RBENV_TEST_DIR}/myproject"
}

@test "no version" {
  assert [ ! -e "${PWD}/.ruby-version" ]
  run rbenv-local
  assert_failure "rbenv: no local version configured for this directory"
}

@test "local version" {
  echo "1.2.3" > .ruby-version
  run rbenv-local
  assert_success "1.2.3"
}

@test "discovers version file in parent directory" {
  echo "1.2.3" > .ruby-version
  mkdir -p "subdir" && cd "subdir"
  run rbenv-local
  assert_success "1.2.3"
}

@test "ignores RBENV_DIR" {
  echo "1.2.3" > .ruby-version
  mkdir -p "$HOME"
  echo "2.0-home" > "${HOME}/.ruby-version"
  RBENV_DIR="$HOME" run rbenv-local
  assert_success "1.2.3"
}

@test "sets local version" {
  mkdir -p "${RBENV_ROOT}/versions/1.2.3"
  run rbenv-local 1.2.3
  assert_success ""
  assert [ "$(cat .ruby-version)" = "1.2.3" ]
}

@test "changes local version" {
  echo "1.0-pre" > .ruby-version
  mkdir -p "${RBENV_ROOT}/versions/1.2.3"
  run rbenv-local
  assert_success "1.0-pre"
  run rbenv-local 1.2.3
  assert_success ""
  assert [ "$(cat .ruby-version)" = "1.2.3" ]
}

@test "unsets local version" {
  touch .ruby-version
  run rbenv-local --unset
  assert_success ""
  assert [ ! -e .ruby-version ]
}
