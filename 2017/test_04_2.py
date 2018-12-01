from pytest import fixture
from aoc_04_passphrase2 import check_validity

@fixture(params=[
    ['abcde fghij', True],
    ['abcde xyz ecdab', False],
    ['a ab abc abd abf abj', True],
    ['iiii oiii ooii oooi oooo', True],
    ['oiii ioii iioi iiio', False],
])
def before(request):
    return request.param

def test_check_validity(before):
    (passphrase, is_valid) = before
    func_is_valid = check_validity(passphrase)
    assert func_is_valid == is_valid, 'Checked validity: %s, actual validity: %s' % (func_is_valid, is_valid)
