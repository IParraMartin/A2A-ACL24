import math
import librosa
import os
import numpy as np
import matplotlib.pyplot as plt
import soundfile as sf

DIRECTORIES = ['/Users/inigoparra/Desktop/sociophonetics/es_peninsular_female/e',
               '/Users/inigoparra/Desktop/sociophonetics/es_peninsular_female/i',
               '/Users/inigoparra/Desktop/sociophonetics/es_peninsular_female/s_v_v',
               '/Users/inigoparra/Desktop/sociophonetics/es_peninsular_male/e',
               '/Users/inigoparra/Desktop/sociophonetics/es_peninsular_male/i',
               '/Users/inigoparra/Desktop/sociophonetics/es_peninsular_male/s_v_v',
               '/Users/inigoparra/Desktop/sociophonetics/es_peruvian_female/e',
               '/Users/inigoparra/Desktop/sociophonetics/es_peruvian_female/i',
               '/Users/inigoparra/Desktop/sociophonetics/es_peruvian_female/s_v_v',
               '/Users/inigoparra/Desktop/sociophonetics/es_peruvian_male/e',
               '/Users/inigoparra/Desktop/sociophonetics/es_peruvian_male/i',
               '/Users/inigoparra/Desktop/sociophonetics/es_peruvian_male/s_v_v'
               ]


def z_sample_generator(directories, scale_factor=0.1):
    counter = 1
    for dir in directories:
        for sample in os.listdir(dir):
            if sample.endswith('.wav'):
                name, _ = os.path.splitext(sample)
                sample_path = os.path.join(dir, sample)
                signal, sr = librosa.load(sample_path)

                RMS = np.sqrt(np.mean(signal ** 2))

                scaling_factor = scale_factor
                z_noise = np.random.normal(0, RMS * scaling_factor, signal.shape[0])

                z_file = signal + z_noise

                output_path = (f'/Users/inigoparra/Desktop/samples/'
                               f'{counter}_noisy_sample_{name}.wav')

                sf.write(output_path, z_file, sr)
                counter += 1

if __name__ == "__main__":
    z_sample_generator(DIRECTORIES)