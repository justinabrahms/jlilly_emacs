#name : select { | ... | ... }
# --
class ${1:model}(models.Model):
    $2

    def __unicode__(self):
        $3
$0
