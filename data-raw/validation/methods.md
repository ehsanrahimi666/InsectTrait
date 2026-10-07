# Segmentation benchmark: methods (auto-generated)
- Subset: 480 crops (10 per species, seed 0) from pollinator-crops-v1, resized to max side 768 px.
- Detectors: none (crop minus 3% margin), yoloworld_x (yolov8x-worldv2), gdino_tiny (IDEA-Research/grounding-dino-tiny), gdino_base (IDEA-Research/grounding-dino-base), owlv2_base (google/owlv2-base-patch16-ensemble), owlvit_base (google/owlvit-base-patch32).
- Prompt sets: {'generic': ['insect'], 'groups': ['bee', 'fly', 'butterfly', 'moth', 'beetle', 'wasp']}; score threshold 0.05; box rule: highest-scoring box centred in the central 60% of the crop, else highest-scoring box; no box -> crop minus 3% margin.
- Segmenters (box-prompted): sam2.1_b, sam2.1_l, sam_b, mobile_sam, fastsam_x, hf_sam_base, grabcut; identical post-processing for all (clip to box + 10%, largest component, holes filled).
- Reference: pixel-wise majority of all combinations except ['grabcut'].
- Metrics: IoU, Dice, boundary F1 (tolerance max(2 px, 1% of equivalent diameter)), false-positive/negative area, mask_ok (detection + >=600 px + <=10% border contact), trait error |trait - reference| / SD(reference) for ['L_mean', 'C_mean', 'vap_wp', 'dark_frac_otsu', 'edge_density']; ICC across methods per trait.
- Best composition: owlv2_base|groups|sam2.1_l (iou_cons 0.932; mask_ok 0.97; 0.49 s per crop).
- Software/hardware: {'python': '3.13.15', 'device': 'Tesla T4', 'torch': '2.11.0+cu128', 'transformers': '5.16.1', 'ultralytics': '8.4.173', 'opencv-python': '4.14.0.94', 'opencv-python-headless': '4.14.0.94', 'numpy': '2.1.3', 'pandas': '2.3.3'}.
