import pytest
from aoc_04_passphrase1 import check_validity

@pytest.fixture(params=[['aa bb cc dd ee', True], ['aa bb cc dd aa', False], ['aa bb cc dd aaa', True]])
def before(request):
    return request.param

def test_check_pass(before):
    (passphrase, is_valid) = before
    func_is_valid = check_validity(passphrase)
    assert func_is_valid == is_valid, 'Checked validity: %s, actual validity: %s' % (func_is_valid, is_valid)
