// JavaScript pour gérer l'affichage de la barre verticale selon l'onglet actif
$(document).ready(function() {
  
  // Fonction pour afficher/masquer la barre verticale
  function toggleVerticalBar() {
    // Vérifier si l'onglet Calculator (shiny-tab-one) est actif
    var calculatorActive = $('.tab-pane#shiny-tab-one').hasClass('active');
    
    // Ajouter ou supprimer la classe pour afficher/masquer la barre
    if (calculatorActive) {
      $('body').addClass('show-vertical-bar');
    } else {
      $('body').removeClass('show-vertical-bar');
    }
  }
  
  // Fonction pour styliser les boutons radio comme des vrais boutons
  function setupCustomRadioButtons() {
    console.log('Initialisation des boutons radio personnalisés...');
    
    // Méthode 1: Écouter les clics sur les labels de réponse
    $(document).off('click.radioLabels').on('click.radioLabels', '.shiny-input-radiogroup .shiny-options-group label, .shiny-input-radiogroup label:not(:first-child)', function(e) {
      var $label = $(this);
      var $input = null;
      
      // Trouver l'input associé à ce label
      if ($label.attr('for')) {
        $input = $('#' + $label.attr('for'));
      } else {
        // Chercher l'input dans le label ou à proximité
        $input = $label.find('input[type="radio"]');
        if ($input.length === 0) {
          $input = $label.next('input[type="radio"]');
        }
        if ($input.length === 0) {
          $input = $label.siblings('input[type="radio"]');
        }
      }
      
      if ($input && $input.length > 0) {
        // Activer l'input radio
        $input.prop('checked', true).trigger('change');
        console.log('Clic sur label, activation de:', $input.attr('name'), 'valeur:', $input.val());
        
        // Mettre à jour immédiatement
        setTimeout(function() {
          updateRadioStyles();
        }, 10);
      }
    });
    
    // Méthode 2: Écouter directement les clics sur les inputs radio
    $(document).off('click.radioInputs').on('click.radioInputs', '.shiny-input-radiogroup input[type="radio"]', function(e) {
      var $clickedInput = $(this);
      var groupName = $clickedInput.attr('name');
      
      console.log('Clic direct sur input radio:', groupName, 'valeur:', $clickedInput.val());
      
      // Laisser le clic normal se produire, puis styliser
      setTimeout(function() {
        updateRadioStyles();
      }, 10);
    });
    
    // Méthode 3: Écouter les changements sur les inputs radio
    $(document).off('change.radioInputs').on('change.radioInputs', '.shiny-input-radiogroup input[type="radio"]', function(e) {
      var $changedInput = $(this);
      var groupName = $changedInput.attr('name');
      
      console.log('Changement sur input radio:', groupName, 'valeur:', $changedInput.val(), 'checked:', $changedInput.prop('checked'));
      
      updateRadioStyles();
    });
    
    // Méthode 4: Observer les mutations pour les changements d'état
    var radioObserver = new MutationObserver(function(mutations) {
      var needsUpdate = false;
      mutations.forEach(function(mutation) {
        if (mutation.type === 'attributes' && mutation.attributeName === 'checked') {
          needsUpdate = true;
        }
      });
      if (needsUpdate) {
        updateRadioStyles();
      }
    });
    
    // Observer tous les inputs radio
    $('.shiny-input-radiogroup input[type="radio"]').each(function() {
      radioObserver.observe(this, { attributes: true, attributeFilter: ['checked'] });
    });
    
    // Méthode 5: Approche alternative spécifique à Shiny
    // Dans Shiny, chaque option radio est souvent dans sa propre div
    $(document).off('click.shinyRadio').on('click.shinyRadio', '.shiny-input-radiogroup .shiny-options-group > *', function(e) {
      var $container = $(this);
      var $input = $container.find('input[type="radio"]');
      
      if ($input.length > 0) {
        // Sélectionner cet input
        $input.prop('checked', true).trigger('change');
        console.log('Clic sur conteneur Shiny, activation de:', $input.attr('name'));
        
        setTimeout(function() {
          updateRadioStyles();
        }, 10);
      }
    });
    
    // Fonction pour mettre à jour tous les styles
    function updateRadioStyles() {
      $('.shiny-input-radiogroup').each(function() {
        var $group = $(this);
        
        // Enlever la classe de tous les labels du groupe (sauf le premier qui est la question)
        $group.find('label:not(:first-child)').removeClass('radio-selected');
        $group.find('.shiny-options-group label').removeClass('radio-selected');
        
        // Ajouter la classe aux labels des inputs sélectionnés
        $group.find('input[type="radio"]:checked').each(function() {
          var $checkedInput = $(this);
          var $label = findLabelForInput($checkedInput);
          
          if ($label && $label.length > 0) {
            $label.addClass('radio-selected');
            console.log('Style appliqué au label pour:', $checkedInput.attr('name'), 'valeur:', $checkedInput.val());
          } else {
            console.log('Aucun label trouvé pour input:', $checkedInput.attr('name'));
          }
        });
      });
    }
    
    // Fonction pour trouver le label associé à un input
    function findLabelForInput($input) {
      var $label = null;
      
      // Méthode 1: par attribut for (plus fiable)
      if ($input.attr('id')) {
        $label = $('label[for="' + $input.attr('id') + '"]');
        if ($label.length > 0) return $label;
      }
      
      // Méthode 2: label précédent (structure Shiny: label puis input)
      $label = $input.prev('label');
      if ($label.length > 0) return $label;
      
      // Méthode 3: dans la structure Shiny, parfois le label enveloppe l'input
      $label = $input.closest('label');
      if ($label.length > 0) return $label;
      
      // Méthode 4: dans le même conteneur (div), chercher le label le plus proche
      var $container = $input.parent();
      $label = $container.find('label').not(':first-child'); // pas le label de la question
      if ($label.length > 0) return $label.first();
      
      // Méthode 5: label dans le même conteneur (chercher le label le plus proche)
      $label = $input.siblings('label').last(); // dernier label trouvé
      if ($label.length > 0) return $label;
      
      // Méthode 6: label parent ou dans conteneur parent
      $label = $input.parent().find('label').last();
      if ($label.length > 0) return $label;
      
      // Méthode 7: Structure alternative Shiny - chercher dans le parent's parent
      var $grandParent = $input.parent().parent();
      if ($grandParent.hasClass('shiny-options-group')) {
        $label = $grandParent.find('label').filter(function() {
          // Trouver le label qui n'est pas le premier enfant du groupe radio
          return !$(this).parent().hasClass('shiny-input-radiogroup') || !$(this).is(':first-child');
        }).first();
        if ($label.length > 0) return $label;
      }
      
      return null;
    }
    
    // Initialiser les styles au démarrage
    setTimeout(updateRadioStyles, 100);
    setTimeout(updateRadioStyles, 500);
    setTimeout(updateRadioStyles, 1000);
    
    // Fonction de debug pour inspecter la structure HTML
    setTimeout(function() {
      console.log('=== DEBUG: Structure HTML des boutons radio ===');
      $('.shiny-input-radiogroup').each(function(i) {
        var $group = $(this);
        console.log('Groupe ' + i + ':', $group.attr('id') || 'sans id');
        console.log('HTML du groupe:', $group.html());
        
        $group.find('input[type="radio"]').each(function(j) {
          var $input = $(this);
          console.log('  Input ' + j + ':', {
            id: $input.attr('id'),
            name: $input.attr('name'),
            value: $input.val(),
            checked: $input.prop('checked')
          });
          
          var $label = findLabelForInput($input);
          console.log('  Label associé:', $label ? $label.text().trim() : 'AUCUN');
        });
      });
    }, 1500);
  }
  
  // Écouter les clics sur les onglets de la sidebar
  $('.sidebar-menu li a').on('click', function() {
    // Attendre un petit délai pour que Shiny traite le changement d'onglet
    setTimeout(toggleVerticalBar, 100);
  });
  
  // Écouter les changements d'onglets dans les tabBox (si nécessaire)
  $(document).on('click', '.nav-tabs li a', function() {
    setTimeout(toggleVerticalBar, 100);
  });
  
  // Initialisation avec délais progressifs pour s'assurer que Shiny a fini de charger
  setTimeout(function() {
    toggleVerticalBar();
    setupCustomRadioButtons();
  }, 500);
  
  setTimeout(function() {
    setupCustomRadioButtons();
  }, 1000);
  
  setTimeout(function() {
    setupCustomRadioButtons();
  }, 2000);
  
  // Observer les changements dans le DOM pour détecter les changements d'onglets et nouveaux éléments
  var observer = new MutationObserver(function(mutations) {
    var needsRadioUpdate = false;
    var needsVerticalBarUpdate = false;
    
    mutations.forEach(function(mutation) {
      if (mutation.type === 'attributes' && mutation.attributeName === 'class') {
        var target = $(mutation.target);
        if (target.hasClass('tab-pane') && (target.attr('id') === 'shiny-tab-one' || target.attr('id') === 'shiny-tab-two')) {
          needsVerticalBarUpdate = true;
        }
      }
      
      // Vérifier si de nouveaux éléments radio ont été ajoutés
      if (mutation.type === 'childList' && mutation.addedNodes.length > 0) {
        $(mutation.addedNodes).each(function() {
          if ($(this).find && $(this).find('.shiny-input-radiogroup, input[type="radio"]').length > 0) {
            needsRadioUpdate = true;
          }
        });
      }
    });
    
    if (needsVerticalBarUpdate) {
      toggleVerticalBar();
    }
    
    if (needsRadioUpdate) {
      setTimeout(setupCustomRadioButtons, 100);
    }
  });
  
  // Commencer à observer
  observer.observe(document.body, {
    attributes: true,
    childList: true,
    subtree: true,
    attributeFilter: ['class']
  });
  
});