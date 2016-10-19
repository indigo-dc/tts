# Ansible setup for the Token Translation service

The real project can be found at https://github.com/indigo-dc/tts.git

## Quickstart
- Install ansible (https://docs.ansible.com/ansible/intro_installation.html)
- Clone this repo
- Create a inventory file, e.g. using the example file:
  ```sh
  cp hosts.example hosts
  ```
- Run the playbook
  ```sh
  ansible-playbook -i hosts site.yml
  ```
